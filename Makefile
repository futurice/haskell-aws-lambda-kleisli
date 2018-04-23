all : Kleisli.zip
	@echo "Now you can upload Kleisli.zip to AWS Lambda!"

.PHONY : all build build-haskell build-python clean example install-ubuntu-dev

clean :
	rm -rf Kleisli.zip build

build : build-python

build-haskell : kleisli.cabal src/Kleisli.hs
	# Build Haskell
	cabal new-build -w ghc-8.2.2
	# Make directories
	mkdir -p haskell/include
	mkdir -p haskell/lib
	# Copy header and .so library
	cp $$(find dist-newstyle/build -type f -name Kleisli_stub.h) haskell/include/kleisli_haskell.h
	cp $$(find dist-newstyle/build -type f -name libkleisli-haskell.so.1.0.0) haskell/lib
	# Make .so links
	ln -fs libkleisli-haskell.so.1.0.0 haskell/lib/libkleisli-haskell.so.1
	ln -fs libkleisli-haskell.so.1.0.0 haskell/lib/libkleisli-haskell.so

build-python : build-haskell kleisli_native.c setup.py
	python setup.py build

Kleisli.zip : build-python Kleisli.py
	# Remove old
	rm -f $@
	# Add files
	cd build/lib.linux-x86_64-2.7 && zip -9 ../../$@ *
	cd haskell/lib && zip -9 ../../$@ *
	ldd haskell/lib/libkleisli-haskell.so | python collect-shared-libs.py $@
	zip -9 $@ Kleisli.py
	# Test
	zip -Tv $@
	# Print (with size) size
	ls -lh $@

# Run an example locally
example :
	PYTHONPATH=build/lib.linux-x86_64-2.7 LD_LIBRARY_PATH=haskell/lib python example.py

# Install some dependencies
# We assume you have ghc-8.2.2 and cabal-install-head installed from hvr-ppa
# https://launchpad.net/~hvr/+archive/ubuntu/ghc
install-ubuntu-dev :
	apt install python-dev
