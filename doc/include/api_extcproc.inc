
While the [cmd critcl::cproc] command understands the most common C
types (see section [sectref {Embedded C Code}]), sometimes this is not
enough.

[para] To get around this limiation the command in this section
enables users of [package critcl] to extend the set of result types
understood by [cmd critcl::cproc]:

[list_begin definitions]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd ::critcl::resulttype] [arg name] [arg body] [opt [arg ctype]]]

This command defines the resulttype [arg name], and associates it with
the C code doing the conversion ([arg body]) to Tcl. The C result type
of the associated function and result variable is [arg ctype], which
defaults to [arg name] if it is not specified.

[para] If [arg name] is declared already an error will be thrown.

[emph Attention!] The result type [const void] is special as it has no accompanying result variable. 

[para] The [arg body]'s responsibility is the conversion of the
functions result into a Tcl result and a Tcl status. The first has to
be set into the interpreter we are in, and the second has to be
returned.

[para] The C code of [arg body] is guaranteed to be called last in the
wrapper around the actual implementation of the [cmd cproc] in
question and has access to the following environment:

[list_begin definitions]
[def [var rv]] The C variable holding the result to convert, of type
               [arg ctype].
[def [var ip]] A Tcl_Interp* typed C variable referencing the interpreter
               the result has to be stored into.
[list_end]

As examples here are the definitions of two standard result types:

[example {
    resulttype int {
	Tcl_SetObjResult(ip, Tcl_NewIntObj(rv));
	return TCL_OK;
    }

    resulttype ok {
	/* ip result must be set by cproc body */
	return rv;
    } int
}]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[call [cmd ::critcl::resulttype] [arg name] [method =] [arg origname]

This form of the [cmd resulttype] command declares [arg name] as an
alias of result type [arg origname], which has to be defined
already. If this is not the case an error is thrown.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[list_end]
