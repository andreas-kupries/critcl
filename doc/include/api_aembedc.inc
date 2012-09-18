
For the advanced user five commands used inside of [cmd critcl::cproc] are
exposed. These are:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argnames] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of the user visible arguments found in the declaration.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argcnames] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of the C side variable names for the user visible arguments
found in the declaration. The names returned here match the names used in the
declarations and code returned by [cmd ::critcl::argvardecls] and
[cmd ::critcl::argconversion].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argcsignature] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of C parameter declarations for all arguments found
in the declaration.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argvardecls] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of C side variable declarations for the user
visible arguments found in the declaration. The names used in these
declarations match the names returned by [cmd ::critcl::argcnames].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argconversion] [arg arguments] [opt [arg n]]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of C code fragments converting the user visible arguments
found in the declaration from Tcl_Obj* to C types. The names used in these
statements match the names returned by [cmd ::critcl::argcnames].

[para] The generated code assumes that the procedure arguments start
at index [arg n] of the [var objv] array. If this argument is not
specified [const 1] will be assumed.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argoptional] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of boolean values indicating which arguments are
optional ([const true]) and not ([const false]).

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argdefaults] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list containing the default values for all optional arguments.

[list_end]
