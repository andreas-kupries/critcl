
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
[call [cmd ::critcl::argconversion] [arg arguments]]

This command takes an argument declaration as taken by [cmd critcl::cproc]
and returns a list of C code fragments converting the user visible arguments
found in the declaration from Tcl_Obj* to C types. The names used in these
statements match the names returned by [cmd ::critcl::argcnames].

[list_end]
