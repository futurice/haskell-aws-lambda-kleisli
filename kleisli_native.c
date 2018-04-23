#include <Python.h>
#include <stdio.h>
#include "HsFFI.h"
#include "haskell/include/kleisli_haskell.h"

// An exported handler. it does minimal job calling into Haskell.
//
// (event : string, context : LambdaContext) :	string
static PyObject* handler(PyObject *self, PyObject *args) {
	const char* event;
	PyObject* context;
	PyObject* logging;

	// https://docs.python.org/2.7/c-api/arg.html
	// first argument is a string, second is a context
	if (!PyArg_ParseTuple(args, "sOO:handler", &event, &context, &logging)) {
		return NULL;
	}

	// Check that third argument is c allable
	if (!PyCallable_Check(logging)) {
		PyErr_SetString(PyExc_TypeError, "3rd parameter must be callable");
		return NULL;
	}

	// TODO: how to log!

	// Run Haskell computation
	char *res = kleisliHaskellHandler(event, context, logging);

	// When memory buffers are passed as parameters to supply data to build objects, as for the s and s# formats, the required data is copied.
	return Py_BuildValue("s", res);
}

// TODO: hs_exit?

// http://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/ffi-chap.html#making-a-haskell-library-that-can-be-called-from-foreign-code
static PyObject *hs_init_wrapper(PyObject *self, PyObject *args) {
	int argc = 0;
	char *argv[] = { NULL };
	char **pargv = argv;
	hs_init(&argc, &pargv);

	// do any other initialization here and
	Py_RETURN_NONE;
}

static PyMethodDef kleisli_methods[] = {
	{
		"handler", handler, METH_VARARGS,
		"AWS Lambda handler"
	},
	{
		"hs_init", hs_init_wrapper, METH_VARARGS,
		"The call to hs_init() initializes GHC's runtime system. Do NOT try to invoke any Haskell functions before calling hs_init(): bad things will undoubtedly happen."
	},
	{NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initkleisli_native(void) {
	(void) Py_InitModule ("kleisli_native", kleisli_methods);
}
