from __future__ import print_function

import json
import kleisli_native

# Init Haskell RTS
kleisli_native.hs_init()

# Call handler and print the result.
input = json.dumps("Sphinx of black quartz, judge my vow")
output = kleisli_native.handler(input, None, print)
print(output)
