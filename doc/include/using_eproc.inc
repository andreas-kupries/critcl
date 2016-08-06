[subsection {A Simple Procedure}]

Starting simple, let us assume that the Tcl code in question is
something like

[example {
    proc math {x y z} {
        return [expr {(sin($x)*rand())/$y**log($z)}]
    }
}]

with the expression pretending to be something very complex and
slow. Converting this to C we get:

[example {
    package require critcl

    critcl::cproc math {double x double y double z} double {
        double up   = rand () * sin (x);
	double down = pow(y, log (z));
        return up/down;
    }
}]

Notable about this translation:

[list_begin enumerated]
[enum] All the arguments got type information added to them, here
       "double".  Like in C the type precedes the argument name. Other
       than that it is pretty much a Tcl dictionary, with keys and
       values swapped.
[enum] We now also have to declare the type of the result, here
       "double", again.
[enum] The reference manpage lists all the legal C types supported as
       arguments and results.
[list_end]
