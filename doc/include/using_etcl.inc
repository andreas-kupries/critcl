[subsection {Which Tcl ?}]

When building the shared library from the embedded C sources one of
the things critcl does for us is to provide the Tcl headers,
especially the stubs declarations.

[para] By default these are the Tcl 8.4 headers and stubs, which
covers 90% of the cases. What when the package in question is meant
for use with Tcl 8.5 or higher, using C-level features of this version
of Tcl.

[para] Use the [cmd critcl::tcl] command to declare to critcl the
minimum version of Tcl required to operate the package. This can be
either [const 8.4], [const 8.5], or [const 8.6], and critcl then
supplies the proper headers and stubs.

[example {
    package require critcl
    critcl::tcl 8.5

    ... Declare your code ...
}]
