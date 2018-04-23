from __future__ import print_function
import kleisli_native
import json

# We init Haskell RTS once when the module is loaded.
kleisli_native.hs_init()

# Handler: function name is what you tell AWS Lambda to execute.
#
# TODO: We json.dumps the payload, and json.loads the result
# as serializing/deserialising via JSON
# is simpler than marshalling Python's objects.
#
def handler(event, context):
    print("Logging from Python")
    return json.loads(kleisli_native.handler(json.dumps(event), context, print))
