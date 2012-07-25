
[list_begin definitions]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd insvariable] [arg ctype] [arg name] [opt [arg comment]] \
     [opt [arg constructor]] [opt [arg destructor]]]

This command specifies a field in the instance structure of the class.

Multiple fields can be specified, and are saved in the order
specified.

[para] [emph Attention:] Specification of an instance variable
precludes the use of an external C [cmd type] for the instance
structure.

[para] [emph Attention:] Specification of an instance variable
automatically causes the definition of an instance variable of type
[const Tcl_Command], and named [const cmd], holding the token of the
instance command, and the definition of an instance method named
[const destroy]. This implicit instance variable is managed by the
system.

[para] Beyond the basic [arg name] and C type of the new variable the
definition may also contain a [arg comment] describing it, and C code
blocks to initialize and release the variable.

These are effectively local forms of the commands [cmd constructor]
and [cmd destructor]. Please read their descriptions for details
regarding the C environment available to the code.

[para] The comment, if specified will be embedded into the generated C
code for easier cross-referencing from generated [file .c] file to
class specification.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd method] [arg name] [arg arguments] [arg body]]

This command specifies an instance method and the C code block
implementing its functionality. This body has access to

[list_begin definitions]
[def [var instance]] Pointer to the instance structure.
[def [var interp]] Pointer to the Tcl interpreter (Tcl_Interp*) the
instance structure is associated with
[def [var objc]] The number of method arguments.
[def [var objv]] The method arguments, as C array of Tcl_Obj pointers.
[list_end]

The [arg arguments] of the definition are only a human readable form
of the method arguments and are not used in the C code, except as
comments. It is still the responsibility of the user to check the
number of arguments, extract them, check their types, etc.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd method] [arg name] [const as] [arg funname] [opt [arg arg]...]]

This variant of the command above defines an instance method of the
class as implemented by the external function [arg funname],
i.e. which is declared outside of the class code itself, or in a
[cmd support] block.

[para] It is assumed that the first four arguments of that function
represent the parameters

[list_begin definitions]
[def [var instance]] Pointer to the instance structure.
[def [var interp]] Pointer to the Tcl interpreter (Tcl_Interp*) the
instance structure is associated with
[def [var objc]] The number of method arguments.
[def [var objv]] The method arguments, as C array of Tcl_Obj pointers.
[list_end]

Any additional arguments specified will be added after these and are
passed into the C code as is, i.e. are considered to be C expressions.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[list_end]
