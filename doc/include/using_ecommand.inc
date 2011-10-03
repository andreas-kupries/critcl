[subsection {Handling A Variable Number Of Arguments}]

In [sectref {A Simple Procedure}] we demonstrated how easy a
translation to C can be. Is it still as easy when we introduce
something moderately complex like handling a variable number of
arguments ? A feature which is needed to handle commands with options
and optional arguments ? Unfortunately not.

We can use [cmd critcl::cproc] only if the number of arguments is
known beforehand, i.e. at the time of declaration. This of course also
means that they do not support default arguments either.

[para]

Thus, to handle something like the example below

[example {
    proc math {args} {
        set sum 0
        foreach y { set sum [expr {$sum + $y}] }
	return $sum
    }
}]

we have to use [cmd critcl::ccommand] instead.

[para] Its advantage: Access to the low-level C arguments representing
the Tcl arguments of the command.

That allows things like variable number of arguments, optional
arguments, options, etc.

[para] Its disadvantage: Access to the low-level C arguments
representing the Tcl arguments of the command.

Where [cmd critcl::cproc] handles the task of converting from Tcl to C
values (for arguments) and back (for the result), with
[cmd critcl::command]s we have to do this on our own.

[para] Here is the translation of the example:

[example {
    package require critcl

    critcl::ccommand math {cd ip oc ov} {
        double sum = 0;
        double y;

        while (oc) {
            if (Tcl_GetDoubleFromObj (ip, ov[oc], &y) != TCL_OK) {
	        return TCL_ERROR;
            }
            sum += y;
	    oc --;
        }

        Tcl_SetObjResult (ip, Tcl_NewDoubleObj (sum));
        return TCL_OK:
    }
}]

Notable about this translation:

[list_begin enumerated]

[enum] As promised/threatened, all the conversions between the Tcl and
       C domains are exposed, and the developer should know her way
       around Tcl's C API.

[enum] The four arguments "cd ip oc ov" are our names for the
low-level arguments holding
[list_begin enumerated]
[enum] ClientData (reference)
[enum] Tcl_Interp (reference)
[enum] Number of arguments, and
[enum] Array of argument values, each a Tcl_Obj*.
[list_end]

This list of arguments, while not optional in itself, is allowed to be
empty, and/or to contain empty strings as argument names. If we do
that critcl will supply standard names for the missing pieces, namely:

[list_begin enumerated]
[enum] clientdata
[enum] interp
[enum] objc
[enum] objv
[list_end]
[list_end]
