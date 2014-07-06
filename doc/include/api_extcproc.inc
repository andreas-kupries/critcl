
While the [cmd critcl::cproc] command understands the most common C
types (see section [sectref {Embedded C Code}]), sometimes this is not
enough.

[para] To get around this limitation the commands in this section
enable users of [package critcl] to extend the set of argument and
result types understood by [cmd critcl::cproc]. In other words, to
define their own custom types.

[comment {
[para] [emph ATTENTION] Starting with Critcl version XXX these
commands are just backward compatibility shims delegating to the
equivalent commands in the core utility package
[package critcl::typeconv].

Users of these commands are asked to update their code.
}]

[list_begin definitions]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd ::critcl::resulttype] [arg name] [arg body] [opt [arg ctype]]]

This command defines the result type [arg name], and associates it
with the C code doing the conversion ([arg body]) from C to Tcl.

The C return type of the associated function, also the C type of the
result variable, is [arg ctype]. This type defaults to [arg name] if
it is not specified.

[para] If [arg name] is declared already an error will be thrown.

[emph Attention!] The standard result type [const void] is special as
it has no accompanying result variable. This cannot be expressed
by the this extension command.

[para] The [arg body]'s responsibility is the conversion of the
functions result into a Tcl result and a Tcl status. The first has to
be set into the interpreter we are in, and the second has to be
returned.

[para] The C code of [arg body] is guaranteed to be called last in the
wrapper around the actual implementation of the [cmd cproc] in
question and has access to the following environment:

[list_begin definitions]
[def [var interp]] A Tcl_Interp* typed C variable referencing the
                   interpreter the result has to be stored into.
[def [var rv]] The C variable holding the result to convert, of type
               [arg ctype].
[list_end]

As examples here are the definitions of two standard result types:

[example {
    resulttype int {
	Tcl_SetObjResult(interp, Tcl_NewIntObj(rv));
	return TCL_OK;
    }

    resulttype ok {
	/* interp result must be set by cproc body */
	return rv;
    } int
}]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd ::critcl::resulttype] [arg name] [method =] [arg origname]]

This form of the [cmd resulttype] command declares [arg name] as an
alias of result type [arg origname], which has to be defined
already. If this is not the case an error is thrown.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd ::critcl::argtype] [arg name] [arg body] [opt [arg ctype]] [opt [arg ctypefun]]]

This command defines the argument type [arg name], and associates it
with the C code doing the conversion ([arg body]) from Tcl to C

The C type of the variable to hold the conversion result is
[arg ctype] and the type of the function argument itself is
[arg ctypefun].  Both types default to [arg name] if they are not
specified (or the empty string).

[para] If [arg name] is declared already an error will be thrown.

[para] The [arg body]'s responsibility is the conversion of a
command's Tcl_Obj* argument into a C value for the underlying
function and its storage in a helper variable.

[para] The C code of [arg body] is guaranteed to be called inside of a
separate C code block (thus allowing the use of local variables) which
has access to the following environment:

[list_begin definitions]
[def [var interp]] A Tcl_Interp* typed C variable referencing the
                   interpreter the code is running in.
[def [const @@]] A placeholder for the Tcl_Obj*-valued C expression
     	    	 providing the value of the argument to convert.

[def [const @A]] A placeholder for the name of the C variable to store the
     	    	 converted argument into.
[list_end]

As examples here are the definitions of two standard argument types:

[example {
    argtype int {
	if (Tcl_GetIntFromObj(interp, @@, &@A) != TCL_OK) return TCL_ERROR;
    }

    argtype float {
	double t;
	if (Tcl_GetDoubleFromObj(interp, @@, &t) != TCL_OK) return TCL_ERROR;
	@A = (float) t;
    }
}]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd ::critcl::argtype] [arg name] [method =] [arg origname]]

This form of the [cmd argtype] command declares [arg name] as an alias
of argument type [arg origname], which has to be defined already. If
this is not the case an error is thrown.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::argtypesupport] [arg name] [arg code]]

This command defines a C code fragment for the already defined
argument type [arg name] which will be inserted before all functions
using that type. Its purpose is the definition of any supporting C
types needed by the argument type.

If the type is used by many functions the system ensure that only the
first of the multiple insertions of the code fragment is active, and
the others disabled.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[list_end]
