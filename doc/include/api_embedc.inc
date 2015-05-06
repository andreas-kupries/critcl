
The package provides five commands to declare various types of C code
fragments. These are:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ccode] [arg text]]

This command compiles the C code in [arg text] and makes the
contained definitions (variables, functions, macros, etc.) available
to all C code fragments specified after it. It itself can assume to
have access to all definitions which were specified before it.

See section [sectref {Runtime Behaviour}] for more details.

[para]
The result of the command is the empty string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ccommand] [arg tclname] [arg cfunname]]

This command creates a new Tcl command named [arg tclname] which is
implemented by the C function [arg cfunname].

It is expected that [arg cfunname] has the proper signature for a Tcl
command function, and was declared already.

[para] The result of [cmd ::critcl::ccommand] itself is the empty
string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::ccommand] [arg tclname] [arg arguments] [arg body] [opt "[arg option] [arg value]..."]]

This form of [cmd critcl::ccommand] creates a new Tcl command named
[arg tclname] which is implemented by the C code in [arg body].

[para] The command wraps the [arg body] in an invisible C function,
compiles it and makes the resulting definition available to all C code
fragments declared later on. It itself can assume to have access to
all definitions which came before it.

See section [sectref {Runtime Behaviour}] for more details.

[para] The result of [cmd critcl::ccommand] itself is the empty
string.

[para] The list of [arg arguments] contain the names for the four
parameters required by a Tcl command function. Superfluous list
elements (i.e. beyond the fourth) are ignored. Missing elements
(parameters), and empty parameter names are handled by replacing them
with standard names. These are, in order of usage

[list_begin enumerated]
[enum] [const clientdata]
[enum] [const interp]
[enum] [const objc]
[enum] [const objv]
[list_end]

The only options accepted by this command are:

[list_begin options]
[opt_def -clientdata [arg c-expression]]

The value of this option is the text of a single C expression. The
value of expression is used in the generated C statement registering
[arg tclname] to initialize the client data of the new Tcl command.

If not specified the expression defaults to [const NULL], i.e. no
client data.

[opt_def -delproc [arg c-expression]]

The value of this option is the text of a single C expression. The
value of this expression has to be a function pointer of type
"Tcl_CmdDeleteProc", which is used in the generated C statement
registering [arg tclname] to initialize a deletion function for the
new Tcl command, i.e. a function which is run by Tcl when the Tcl
command is deleted again.

If not specified the expression defaults to [const NULL], i.e. no
deletion function.

[opt_def -cname [arg boolean]]

The value of this option is a boolean flag. If [const true] the name
of the command is the C identifier of the command function.
Namespaces, etc. are in that case not relevant at all. The default
value of this option is [const false], causing the system to derive a
name from the Tcl level command name, including its namespace.

[list_end]

A ccommand is, in comparison to functions defined via
[cmd critcl::cproc], more lower level.

Its advantage is that the developer [emph can] do their own argument
processing, enabling things like variable number of arguments,
options, etc., i.e. much higher flexibility.

Their disadvantage is that you [emph have] to do your own argument
processing. Where a [cmd critcl::cproc] generates the code to convert
from Tcl values to C values and back a [cmd critcl::ccommand] forces
the writer to do all of this on their own. I.e. the cost of the
aforementioned flexibility is a higher complexity seen by the user.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cdata] [arg tclname] [arg data]]

This command creates a new Tcl command named [arg tclname] which returns
[arg data] as a ByteArray result.

[para] The result of [cmd critcl::cdata] itself is the empty string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cconst] [arg tclname] [arg resulttype] [arg value]]

This command creates a new Tcl command named [arg tclname] which returns
the constant [arg value] as its result, with Tcl type [arg resulttype].

[para] The result of [cmd critcl::cconst] itself is the empty string.

[para] The command is similar to [cmd critcl::cdata] in that it returns a
constant value. Contrary to [cmd critcl::cdata] however it is not restricted
to ByteArray results, but accepts all result-types known to [cmd critcl::cproc].
Its semantics are actually equivalent to

[example {
    cproc $tclname {} $resulttype "return $value ;"
}]

[para] Contrary to [cmd critcl::cproc] however it is internally optimized to
avoid generating a superfluous C function.

[para] Note that nothing prevents the user from using a C define for the
[arg value]. Any visible C function is actually also allowed, as long as
it does not take arguments.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cdefines] [arg definitions] [opt [arg namespace]]]

This command creates Tcl variables in the specified [arg namespace]
which are linked to the [term {C enum values}] and [term #define]s
named as [term {glob patterns}] in the list of [arg definitions]. Each
variable has the same name as the definition which gave rise to it,
and its value is the value of the corresponding enum value or
#define. The namespace defaults to the global namespace, i.e. "::", if
it wasn't specified explicitly.

[para] Please note that this command is only for the lifting of
[emph existing] C definitions into Tcl. The command [emph {does not}]
create the definitions in C. It actually goes so far to check for the
presence of the named definitions and not performing the mapping for
any which do not exist. Which is sensible, given that non-existing
defines have no value which could be used in the mapping.

[para] As these checks are run at the time the embedded C code of a
[file .critcl] file is actually compiled they have access to and check
[emph all] C fragments defined with [cmd critcl::ccode], plus all the
headers it has access to via [cmd critcl::cheaders], for that file.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cproc] [arg name] [arg arguments] [arg resulttype] [arg body] \
      [opt "[arg option] [arg value]..."]]

This command creates a new Tcl command named [arg tclname] which is
implemented by the C code in [arg body].

In contrast to the low-level [cmd critcl::ccommand] here the arguments
and result are typed and critcl generates the code converting from
Tcl_Obj's to C data types, and vice versa.

The command creates two invisible C functions, one wrapping the
[arg body], the other a shim containing the necessary conversions,
compiles them and makes the resulting definitions available to all C
code fragments declared later on. It itself can assume to have access
to all definitions which came before it.

See section [sectref {Runtime Behaviour}] for more details.

[para] The result of [cmd critcl::cproc] itself is the empty string.

[para] The only options accepted by this command are:

[list_begin options]
[opt_def -cname [arg boolean]]

The value of this option is a boolean flag. If [const true] the name
of the command is the C identifier of the command function.
Namespaces, etc. are in that case not relevant at all. The default
value of this option is [const false], causing the system to derive a
name from the Tcl level command name, including its namespace.

[opt_def -pass-cdata [arg boolean]]

The value of this option is a boolean flag. If specified and set the
shim translating from Tcl to C level and back will pass the command's
ClientData to the function.

If not specified the flag defaults to [const false], i.e. no passing
of client data.

[opt_def -arg-offset [arg int]]

The value of this option is a positive integer number specifying the
number of hidden arguments preceding the actual procedure
arguments.

If not specified the flag defaults to [const 0].

This is useful to higher-order code generator using the command in
settings with prefix arguments which are not directly seen by the
function, but influence argument counting and extraction.

[list_end]

The list below shows the values which are legal for [arg resulttype],
and details their semantics:

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%}]
[list_begin definitions]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def Tcl_Obj*]
[def object]
The function returns a value of type "Tcl_Obj*".
This value becomes the interpreter result, if not [const 0].
The Tcl status is TCL_ERROR when a [const 0] is returned, and TCL_OK otherwise.

[para][emph Attention]: The conversion assumes that the value belonged to
the function, with an associated reference count, and decrements the
reference count to indicate the loss of ownership by the
function. This means that it is an error to return a value whose
reference count is zero.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def char*]
[def vstring]
The function returns a value of type "char*".
This value becomes the interpreter result, wrapped in a String.

It is assumed that the string is volatile in some way, with the
wrapping in a String duplicating it before making it the result,
ensuring that we will not access a dangling pointer in the future.

The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def {const char*}]
Like type [const char*] above, except that the returned string is const-qualified.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def string]
[def dstring]
The function returns a value of type "char*".

Contrary to the previous string types here it is assumed that the
value is dynamically allocated, via [fun Tcl_Alloc].

This value becomes the interpreter result, as usual, but is [emph not] copied.
The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double]
The function returns a value of type "double".
This value becomes the interpreter result, properly wrapped (Int).
The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def float]
The function returns a value of type "float".
This value becomes the interpreter result, properly wrapped (Double).
The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def boolean]
[def bool]
The function returns a value of type "int", interpreted as boolean.
This value becomes the interpreter result, properly wrapped (Int).
The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def int]
The function returns a value of type "int".
This value becomes the interpreter result, properly wrapped (Int).
The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def long]
The function returns a value of type "long int".
This value becomes the interpreter result, properly wrapped (Long).
The Tcl status is always TCL_OK.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def ok]
The function returns a value of type "int".
It is interpreted as the Tcl status code.
The interpreter result is left untouched (empty).

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def void]
The function does not return a value.
The interpreter result is left untouched (empty).
The Tcl status is always TCL_OK.

[list_end]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%}]

[para] Please note that it is possible to extend the above with custom
types if these types are not enough. See section
[sectref {Advanced: Extending cproc}] for details.

[para] The [arg arguments] parameter has the overall syntax of a Tcl
dictionary value, except that keys (argument names) and values
(argument types) are specified in reverse order. Consider the
example [example {
int x int y
}] where [arg x] and [arg y] are the arguments/keys, both
mapped to type/value [const int].

[para] The argument names must be valid C identifiers.

[para] A limited form of variadic arguments is possible, through
optional arguments with default values. For these the argument name is
a 2-element list containing the actual name, and the default value.

For example, in the declaration [example { int {x 1} }] [arg x] is an
optional argument of type [type int] and default value [const 1].

[para] One limitation, and one caveat!

[para] First, the set of optional arguments must be a single
contiguous segment in the argument list. This limits them to a series
of optional arguments at either the beginning, end, or middle of the
list. Multiple segments separated by non-optional arguments are
rejected, as the system cannot determine in these cases which
arguments are present and what to set where.

[para] Second, the default value is assigned unconditionally. If a
custom argument type uses more complex validation, and the default may
be invalid according to it, then the relevant checks have to be done
in the procedure body. The argument conversion cannot do it as it is
completely bypassed when the argument is not present. Overcoming this
requires the separation of argument conversion and validation code.

[para] The list below shows the values which are legal for argument
types, and details their semantics:

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%}]
[list_begin definitions]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def Tcl_Obj*]
[def object]
The function takes an argument of type "Tcl_Obj*".
No argument checking is done.
The Tcl level word is passed to the argument as-is.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def pstring]
The function takes an argument of type "critcl_pstring" containing the
original Tcl_Obj* reference of the Tcl argument, plus the length of
the string and pointer to the characters.

[example {
typedef struct critcl_pstring {
    Tcl_Obj* o;
    char*    s;
    int      len;
} critcl_pstring;
}]

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def list]
The function takes an argument of type "critcl_list" containing the
original Tcl_Obj* reference of the Tcl argument, plus the length of
the Tcl list and pointer to the list elements.

[example {
typedef struct critcl_list {
    Tcl_Obj*  o;
    Tcl_Obj** v;
    int       c;
} critcl_list;
}]

The Tcl argument must be convertible to List, an error is thrown
otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def bytearray]
[def rawchar*]
[def rawchar]
The function takes an argument of type "char*".

The Tcl argument must be convertible to ByteArray, an error is thrown
otherwise.

[emph Note] that the length of the ByteArray is [emph not] passed to
the function.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def char*]
The function takes an argument of type "char*".
The string representation of the Tcl argument is passed in.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double]
The function takes an argument of type "double".

The Tcl argument must be convertible to Double, an error is thrown
otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def float]
The function takes an argument of type "float".

The Tcl argument must be convertible to Double, an error is thrown
otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def boolean]
[def bool]
The function takes an argument of type "int".

The Tcl argument must be convertible to Boolean, an error is thrown
otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def int]
The function takes an argument of type "int".

The Tcl argument must be convertible to Int, an error is thrown
otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def long]
The function takes an argument of type "long int".

The Tcl argument must be convertible to Long, an error is thrown
otherwise.

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def void*]

[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%%}]
[def double*]
[def float*]
[def int*]

The function takes an argument of the same-named C type.

The Tcl argument must be convertible to ByteArray, an error is thrown
otherwise.

The bytes in the ByteArray are then re-interpreted as the raw
representation of a C pointer of the given type which is then passed
as argument to the function.

In other words, this is for Tcl values somehow holding raw C pointers,
i.e. memory addresses.

[para] [emph Attention]: These types are considered [emph DEPRECATED].
It is planned to remove their documentation in release 3.2, and their
implementation in release 3.3.  Their deprecation can be undone if
good use cases are shown.

[list_end]
[comment {% % %% %%% %%%%% %%%%%%%% %%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%}]

[para] [emph Note] that optional arguments are not possible. This
restriction is inherited from C.

[para] Further note that the type of the first argument is allowed to
be [const Tcl_Interp*]. In that case the argument in question is
[emph not] counted as an argument of the new Tcl command.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cproc] [arg name] [arg arguments] [arg resulttype]]

This variant of [cmd critcl::cproc] assumes that the functionality to
connect is implemented by the C function [arg name] which has the
signature described by the [arg arguments] and [arg resulttype].

[para] It creates only the shim performing the conversions required by
arguments and result.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cinit] [arg text] [arg externals]]

This command compiles the C code in [arg text] and [arg externals].

[para] Both have access to all definitions created by the previously
listed commands, regardless of their and its placement in the
[file .critcl] file. See section [sectref {Runtime Behaviour}] for
more details.

[para] The C code in [arg text] is put into the body of the
initialization function of the shared library backing the
[file .critcl] file, and is executed when this library is loaded into
the interpreter.

[para] This code has access to a variable [var interp] of type
[const Tcl_Interp*] referencing the Tcl interpreter currently being
initialized.

[para] The code in [arg externals] on the other hand is placed outside
and just before the initialization function, making this is a good
place for any external symbols required by initialization function
which should not be accessible by any other parts of the C code.

[para]
The result of the command is the empty string.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::include] [arg path]]

This command is a convenient shorthand for

[example {
critcl::code {
  #include <${path}>
}
}]

[comment ---------------------------------------------------------------------]
[list_end]
