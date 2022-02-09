[subsection {Finding external libraries}]

When creating a package exposing some third-party library to Tcl
[sectref {Finding header files}] is only the first part, to enable
failure-free compilation. We also have to find the library/ies
themselves so that they can be linked to our package. This is
described here. The last issue, [sectref {Lifting constants}] from C
to Tcl for the use by scripts is handled in a separate section and
example.

[para] The relevant command is [cmd critcl::clibraries]. Its basic
semantics are like that of [cmd critcl::cheaders], i.e. it enables us
to tell the linker the path(s) where the required libraries can be
found, using

[example {
    critcl::clibraries -L/path/to/libraries/
}]

name them

[example {
    critcl::clibraries -lfoo
}]

or tell it directly which libraries we are using and where they live:

[example {
    critcl::clibraries /path/to/library/foo.so
}]

This last way of using should be avoided however, as it intermingles
searching and naming, plus the name is platform dependent.

[para] [strong Note] that all the above things can be done at the same
time, i.e. in a single command. Further note that the information from
multiple invokations is aggregated by the system.

[example {
    critcl::clibraries -L/path/to/libraries/ -lfoo /path/to/library/snafu.so
}]

[example {
    critcl::clibraries -L/path/to/libraries/ -lfoo
    critcl::clibraries /path/to/library/snafu.so
}]

[para] For OS X we additionally have the [cmd critcl::framework]
command which enables us to name the frameworks used by our package.
Note that this command can be used unconditionally. If the build
target is not OS X it is ignored.
