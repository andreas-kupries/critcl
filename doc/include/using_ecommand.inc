[subsection {Handling A Variable Number Of Arguments}]

In [sectref {A Simple Procedure}] we demonstrated how easy a
translation to C can be. Is it still as easy when we introduce
something moderately complex like handling a variable number of
arguments ? A feature which is needed to handle commands with options
and optional arguments ?

[para] Well, starting with version 3.1.16 [cmd critcl::cproc] does
have full support for optional arguments, [arg args]-style variadics,
and default values, extending its range to everything covered by the
builtin [cmd proc]. The only thing not truly supported are options
(i.e. flag arguments) of any kind.

[para] For the moment, and the example, let us pretend that we can use
[cmd critcl::cproc] only if the number of arguments is fully known
beforehand, i.e. at the time of declaration.

Then we have to use [cmd critcl::ccommand] to handle the translation
of the procedure shown below:

[example {
    proc math {args} {
        set sum 0
        foreach y $args { set sum [expr {$sum + $y}] }
        return $sum
    }
}]

[para] Its advantage: Access to the low-level C arguments representing
the Tcl arguments of the command. Full control over argument
conversion, argument validation, etc.

[para] Its disadvantage: Access to the low-level C arguments
representing the Tcl arguments of the command. Assuming the burden of
having to write argument conversion, argument validation, etc. Where
[cmd critcl::cproc] handles the task of converting from Tcl to C
values (for arguments) and back (for the result), with
[cmd critcl::command] it is the developer who has to write all this
code.

[para] Under our restriction the translation of the example is:

[example {
    critcl::ccommand math {cd ip oc ov} {
        double sum = 0;
        double y;

        oc --;
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

[para] Now, letting go of our pretenses regarding the limitations of
[cmd critcl::cproc], due to the support it does have for
[arg args]-style variadics (since version 3.1.16) we can write a much
simpler translation:

[example {
    critcl::cproc math {double args} double {
        double sum = 0;

        args.c --;
        while (args.c) {
            sum += args.v[args.c];
            args.c --;
        }
        return sum;
    }
}]
