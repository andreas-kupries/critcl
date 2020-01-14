
[list_begin definitions]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def Tcl_Obj*]
[def object]

If the returned Tcl_Obj is [const NULL], the Tcl return code is
[const TCL_ERROR] and the function should [uri \
	https://www.tcl-lang.org/man/tcl/TclLib/SetResult.htm \
	{set an error mesage}]
as the interpreter result.  Otherwise, the returned Tcl_Obj is set as the
interpreter result.

[para]The returned Tcl_Obj should have a reference count greater than zero
because the converter, assuming that the function incremented the reference
count of the returned Tcl_Obj, decrements the reference count to release
possession after setting the interpreter result.  If a Tcl_Obj with a reference
count of zero were returned, the reference count would becomes one when set as
the interpreter result, and immediately thereafter decremented to zero, causing
the memory to be freed.  The system is then likely to crash at some point after
the return due to reuse of the freed memory.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def Tcl_Obj*0]
[def object0]

Like [const Tcl_Obj*] except that this conversion assumes that the
returned value has a reference count of [const 0] and [emph {does not}]
decrement it. Returning a value whose reference count is greater than zero is
likely to cause a memory leak.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def new-channel]

A [type String] Tcl_Obj holding the name of the returned
[type Tcl_Channel] is set as the interpreter result.

The channel is further assumed to be [strong new], and therefore
registered with the interpreter to make it known.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def known-channel]

A [type String] Tcl_Obj holding the name of the returned
[type Tcl_Channel] is set as the interpreter result.

The channel is further assumed to be [strong {already registered}]
with the interpreter.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def char*]
[def vstring]

A [type String] Tcl_Obj holding a copy of the returned [type char*] is set as
the interpreter result.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def {const char*}]

Like [const char*] above, except that the returned string is
[const const]-qualified.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def string]
[def dstring]

The returned [type char*] is directly set as the interpreter result without
making a copy.  Therefore it must be dynamically allocated via [fun Tcl_Alloc].


[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double]
[def float]

The returned [type double] or [type float] is converted to a [type Double]
Tcl_Obj and set as the interpreter result.


[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def boolean]
[def bool]

The returned [type int] value is converted to an [type Int] Tcl_Obj and set as
the interpreter result.


[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def int]

The returned [type int] value is converted to an [type Int] Tcl_Obj and set as
the interpreter result.


[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def long]

The returned [type {long int}] value is converted to a [type Long] Tcl_Obj and
set as the interpreter result.


[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def wideint]

The returned [type Tcl_WideInt] value is converted to a [type WideInt] Tcl_Obj
and set as the interpreter result.


[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def ok]

The returned [type int] value becomes the Tcl return code.

The interpreter result is left untouched and can be set by the function if
desired.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def void]

The function does not return a value.

The interpreter result is left untouched and can be set by the function if
desired.

[list_end]
