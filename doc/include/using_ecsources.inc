[subsection {Separate C sources}]

In all of the examples shown so far the C code was fully embedded in a
[file .critcl] file. However, if the C part is large it can make sense
to break it out of the [file .critcl] file into one or more separate
proper [file .c] file(s).

[para] The [cmd critcl::csources] command can then be used to make
this code known to the original [file .critcl] file again.

This command accepts the paths to the [file .c] files as arguments,
and glob patterns as well.

Our example comes from the [package struct::graph] package in
Tcllib.

Its core C functions are in separate files, and the [file .critcl]
code then makes them known via:

[example {
namespace eval ::struct {
    # Supporting code for the main command.
    critcl::cheaders graph/*.h
    critcl::csources graph/*.c

    ...
}
}]

which tells critcl that these files are in the subdirectory
[file graph] relative to the location of [file graph_c.tcl], which is
the relevant [file .critcl] file.

[para] This example also demonstrates again the use of

[cmd critcl::cheaders], which we also saw in section
[sectref {Finding header files}].
