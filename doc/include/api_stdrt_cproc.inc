
[list_begin definitions]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def Tcl_Obj*]
[def object]

The function returns a value of type [type Tcl_Obj*].

This value becomes the interpreter result, if not [const 0]. The Tcl
status is [const TCL_ERROR] when a [const 0] is returned, and
[const TCL_OK] otherwise.

[para][emph Note] how it is the responsibility of the function to set
the interpreter result to an appropriate error message when returning
[const 0].

[para][emph Attention]: The conversion assumes that the value belonged
to the function, with an associated reference count, and decrements
the reference count to indicate the loss of ownership by the
function. This means that it is an error to return a value whose
reference count is zero. The system will crash at some point after the
return due to reuse of the freed memory.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def Tcl_Obj*0]
[def object0]

The function returns a value of type [type Tcl_Obj*].

This value becomes the interpreter result, if not [const 0]. The Tcl
status is [const TCL_ERROR] when a [const 0] is returned, and
[const TCL_OK] otherwise.

[para][emph Note] how it is the responsibility of the function to set
the interpreter result to an appropriate error message when returning
[const 0].

[para][emph Attention]: Contrary to [const Tcl_Obj*] above this
conversion assumes that the returned value has a reference count of
[const 0] and performs [emph no] decrement. Returning a value whose
reference count is greater than zero will likely cause a memory leak.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def char*]
[def vstring]

The function returns a value of type [type char*].

This value becomes the interpreter result, wrapped in a [type String].

It is assumed that the string is volatile in some way, with the
wrapping in a [type String] duplicating it before making it the
result, ensuring that we will not access a dangling pointer in the
future.

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def {const char*}]

Like type [const char*] above, except that the returned string is
[const const]-qualified.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def string]
[def dstring]

The function returns a value of type [type char*].

Contrary to the previous string types here it is assumed that the
value is dynamically allocated, via [fun Tcl_Alloc].

This value becomes the interpreter result, as usual, but is [emph not]
copied.

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double]

The function returns a value of type [type double].

This value becomes the interpreter result, properly wrapped as a
[type Double].

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def float]

The function returns a value of type [type float].

This value becomes the interpreter result, properly wrapped as a
[type Double].

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def boolean]
[def bool]

The function returns a value of type [type int], interpreted as boolean.

This value becomes the interpreter result, properly wrapped as a
[type Int].

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def int]

The function returns a value of type [type int].

This value becomes the interpreter result, properly wrapped as a
[type Int].

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def long]

The function returns a value of type [type {long int}].

This value becomes the interpreter result, properly wrapped as a
[type Long].

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def wideint]

The function returns a value of type [type Tcl_WideInt].

This value becomes the interpreter result, properly wrapped as a
[type WideInt].

The Tcl status is [emph always] [const TCL_OK].

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def ok]

The function returns a value of type [type int].

It is interpreted as the Tcl status code.

The interpreter result is left untouched (empty).  For a different
outcome the function has to set the interpreter result by itself.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def void]

The function does not return a value.

The interpreter result is left untouched (empty). For a different
outcome the function has to set the interpreter result by itself.

The Tcl status is [emph always] [const TCL_OK].

[list_end]
