[subsection {Having both C and Tcl functionality}]

Often enough only pieces of a package require recoding in C to boost
the whole system. Or, alternatively, the package in question consists
of a low-level layer C with a Tcl layer above encoding policies and
routing to the proper low-level calls, creating a nicer (high-level)
API to the low-level functionality, etc.

[para] For all of this we have to be able to write a package which
contains both C and Tcl, nevermind the fact the C parts are embedded
in Tcl.

[para] The easiest way to structure such a package is to have several
files, each with a different duty. First, a [file .critcl] file
containing the embedded C, and second one or more [file .tcl] files
providing the Tcl parts.

Then use the [cmd critcl::tsources] command in the [file .critcl] file
to link the two parts together, declaring the [file .tcl] files as
necessary companions of the C part.

[example {
    package require critcl

    critcl::tsources your-companion.tcl ; # Companion file to use

    ... embedded C via critcl commands ...
}]

With a declaration as shown above the companion file will be
automatically [cmd source]d when the C parts are made available, thus
making the Tcl parts available as well.
