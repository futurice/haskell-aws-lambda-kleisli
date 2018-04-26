{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | A helper to wrap foreign library .so as AWS Lambda package.
--
-- > -- Find foreign-lib artifact
-- > Just fl <- `findForeignLib` Nothing "kleisli-haskell"
-- >
-- > -- Provide some configuration
-- > let conf = `mkConf'` fl "Kleisli" "kleisliHaskellHandler" "handler"
-- >
-- > -- Create Zip ByteString
-- > bsl <- `packageAwsLambda` conf
-- >
-- > -- And write it to disk, e.g.
-- > BSL.writeFile "Kleisli.zip" bsl
--
module PackageAwsLambda (
    packageAwsLambda,
    Conf (..),
    mkConf,
    mkConf',
    findForeignLib,
    -- * Internal
    compilePython,
    ) where

import           Prelude                    ()
import           Prelude.Compat

import           Control.Applicative        (many, optional, some, (<|>))
import           Control.DeepSeq            (force)
import           Control.Exception          (evaluate)
import           Data.Aeson                 (Value, object, (.=))
import           Data.Char                  (isAlphaNum, isHexDigit)
import           Data.Foldable              (toList)
import           Data.List                  (intercalate, isPrefixOf)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Text                  (Text)
import           Data.Traversable           (for)
import           System.Directory           (copyFile)
import           System.FilePath.Posix      (takeFileName, (</>))
import           System.IO.Temp             (withTempDirectory)
import           System.Process             (proc, readCreateProcess)

import qualified Cabal.Plan                 as CP
import qualified Codec.Archive.Zip          as Zip
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Text.Lazy.IO          as TL
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Process             as Proc
import qualified Text.Microstache           as Mu
import qualified Text.Trifecta              as Tri
import qualified System.FilePath.Glob as Glob

import           Orphans                    ()

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

data Conf = Conf
    { confForeignLib     :: !FilePath
      -- ^ Either a component name or a filepath
    , confPythonModule   :: !Text
      -- ^ Name for resulting python module
    , confHandlers       :: !(NonEmpty (Text, Text))
      -- ^ Handlers as pair of names, Haskell and python name
    , confAdditionalLibs :: ![String]
      -- ^ Additional c-libs to (be considered to) bundle
    , confRtsFlags       :: ![Text]
      -- ^ RTS flags passed to lambda. Setting @-M2G is probably a good idea.
      --
      -- See <https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/runtime_control.html>
    }
  deriving Show

-- | Make 'Conf' from foreign-lib location, Python module name and handlers.
mkConf :: FilePath -> Text -> NonEmpty (Text, Text) -> Conf
mkConf fl pm hs = Conf fl pm hs [] []

-- | Make 'Conf' from foreign-lib location, Python module name and single handler
mkConf' :: FilePath -> Text -> Text -> Text -> Conf
mkConf' fl pm hs py = Conf fl pm ((hs, py) :| []) [] []

-------------------------------------------------------------------------------
-- Find foreign lib
-------------------------------------------------------------------------------

-- | Find foreign lib based on the plan.json
findForeignLib
    :: Maybe FilePath  -- ^ Optional build dir to look in
    -> Text            -- ^ Component name
    -> IO (Maybe FilePath)
findForeignLib bd cn = do
    (plan, _) <- CP.findAndDecodePlanJson bd

    let mdir = listToMaybe
            [ d
            | u <- Map.elems (CP.pjUnits plan)
            , d <- toList (CP.uDistDir u)
            , (CP.CompNameFLib cn', _) <- Map.toList (CP.uComps u)
            , cn' == cn
            ]

    case mdir of
        Nothing  -> return Nothing
        Just dir -> listToMaybe <$>
            Glob.globDir1 (Glob.compile $ dir </> "**" </> "*.so") "/"

-------------------------------------------------------------------------------
-- Templates
-------------------------------------------------------------------------------

templateC :: Mu.Template
templateC = $(TH.qRunIO (Mu.compileMustacheFile "templates/module.c") >>= TH.lift)

templatePy :: Mu.Template
templatePy = $(TH.qRunIO (Mu.compileMustacheFile "templates/module.py") >>= TH.lift)

templateSetup :: Mu.Template
templateSetup = $(TH.qRunIO (Mu.compileMustacheFile "templates/setup.py") >>= TH.lift)

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

renderC :: Conf -> TL.Text
renderC = Mu.renderMustache templateC . toValue

renderPy :: Conf -> TL.Text
renderPy = Mu.renderMustache templatePy . toValue

renderSetup :: Conf -> TL.Text
renderSetup = Mu.renderMustache templateSetup . toValue

toValue :: Conf -> Value
toValue conf = object
    [ "foreignLib"       .= either (const "???") id
        (libraryName (confForeignLib conf))
    , "nativeModuleName" .= (confPythonModule conf <> "_native")
    , "handlers" .=
        [ object [ "hs" .= hs, "py" .= py ]
        | (hs, py) <- toList (confHandlers conf)
        ]
    , "rtsFlags" .= intercalate ", " (map show (confRtsFlags conf))
    ]

-------------------------------------------------------------------------------
-- Name of libraries
-------------------------------------------------------------------------------

-- | Deduce library name from filename of dynamic library
--
-- >>> libraryName "libkleisli.so.1.0.0"
-- Right "kleisli"
--
libraryName :: FilePath -> Either String String
libraryName = resultToEither . Tri.parseString p mempty . takeFileName
  where
    p = do
        _ <- Tri.string "lib"
        s <- many (Tri.notChar '.')
        Tri.skipMany Tri.anyChar
        return s

resultToEither :: Tri.Result a -> Either String a
resultToEither (Tri.Success x) = Right x
resultToEither (Tri.Failure e) = Left (show (Tri._errDoc e))

-------------------------------------------------------------------------------
-- Python compilation
-------------------------------------------------------------------------------

compilePython :: FilePath -> Conf -> IO FilePath
compilePython tmpDir conf = do
    ln <- either fail return $ libraryName flibFileName
    let so = tmpDir </> "lib" <> ln <> ".so"
    copyFile flibFileName so
    TL.writeFile (tmpDir </> "setup.py") setupContents
    TL.writeFile (tmpDir </> cFileName) cContents
    _ <- readCreateProcess p ""
    return $ tmpDir </> "build" </> "lib.linux-x86_64-2.7" </> T.unpack (confPythonModule conf <> "_native.so")
  where
    flibFileName  = confForeignLib conf
    cFileName     = T.unpack $ confPythonModule conf <> "_native.c"
    cContents     = renderC conf
    setupContents = renderSetup conf

    p = (proc "python2.7" ["setup.py", "build"])
        { Proc.cwd = Just tmpDir
        }

-------------------------------------------------------------------------------
-- LDD magic - find dependencies
-------------------------------------------------------------------------------

findExtraLibs
    :: [String]        -- ^ additional libs
    -> FilePath
    -> IO [FilePath]
findExtraLibs additionalCopyLibs fp = do
    output <- readCreateProcess (proc "ldd" [fp]) ""
    either fail (return . catMaybes) $ resultToEither $
        Tri.parseString (many lddLine <* Tri.eof) mempty output
  where
    lddLine = do
        Tri.spaces
        lib <|> ldLinux

    lib = do
        l :| _ <- Tri.sepByNonEmpty
            (some $ Tri.satisfy $ \c -> isAlphaNum c || c == '-')
            (Tri.char '.')

        Tri.spaces
        md <- optional $ do
            _ <- Tri.string "=>"
            Tri.spaces
            many $ Tri.satisfy $ \c ->
                isAlphaNum c || c == '.' || c == '-' || c == '_' || c == '/'
        _ <- address

        if | "libHS" `isPrefixOf` l -> return md
           | l `elem` skipLibs      -> return Nothing
           | l `elem` copyLibs      -> return md
           | otherwise              -> fail $ "Unknown lib " ++ l

    ldLinux = Tri.string "/lib64/ld-linux-x86-64.so.2" *> address *> return Nothing

    -- Libraries which exist in Linux AMI
    skipLibs =
        [ "librt"
        , "libutil"
        , "libdl"
        , "libpthread"
        , "libm"
        , "libc"
        , "linux-vdso"
        ]

    -- Libraries which we know for sure aren't in Amazon Linux AMI
    copyLibs =
        [ "libgmp"
        , "libffi"
        ] ++ additionalCopyLibs

    address = Tri.spaces
        *> Tri.char '('
        *> Tri.string "0x"
        *> Tri.skipMany (Tri.satisfy isHexDigit)
        *> Tri.char ')'
        *> Tri.char '\n'

-------------------------------------------------------------------------------
-- Build Zip
-------------------------------------------------------------------------------

-- | Package AWS Lambda package.
packageAwsLambda
    :: Conf               -- ^ configuration
    -> IO BSL.ByteString  -- ^ Result is ZIP package
packageAwsLambda conf =
    withTempDirectory "/tmp" "aws-lambda-py" $ \tmpDir -> do
        -- python native extension
        pySo <- compilePython tmpDir conf
        pySoContents <- BSL.readFile pySo
        let pySoEntry = Zip.toEntry (takeFileName pySo) 0 pySoContents

        -- haskell
        hsSoContents <- BSL.readFile hsFileName
        let hsSoEntry = Zip.toEntry (takeFileName hsFileName) 0 hsSoContents

        -- .so dependencies
        libs <- findExtraLibs (confAdditionalLibs conf) hsFileName
        libEntries <- for libs $ \lib -> do
            libContents <- BSL.readFile lib
            return $ Zip.toEntry (takeFileName lib) 0 libContents

        -- all together
        let entries = pyEntry : pySoEntry : hsSoEntry : libEntries

        evaluate $ force $ Zip.fromArchive Zip.Archive
            { Zip.zEntries   = entries
            , Zip.zSignature = Nothing
            , Zip.zComment   = mempty
            }
  where
    hsFileName = confForeignLib conf

    pyFileName = T.unpack $ confPythonModule conf <> ".py"
    pyContents = renderPy conf
    pyEntry    = Zip.toEntry pyFileName 0 $ TLE.encodeUtf8 pyContents
