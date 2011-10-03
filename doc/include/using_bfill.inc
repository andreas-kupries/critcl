[subsection {Pre-Filling The Result Cache}]

The default mode of the [cmd critcl] application is to take a series
of [file .critcl] files, build their binaries, and leave them behind
in the result cache. When the files are later actually used the
compile and link steps can be skipped, leading to shorter load times.

[para]

The command line for this is

[example {
    critcl foo.tcl
}]

or, to process multiple files

[example {
    critcl foo.tcl bar.tcl ...
}]

One thing to be aware of, should [cmd critcl] find that the cache
already contains the results for the input files, no building will be
done. If you are sure that these results are outdated use the option
[option -force] to force(sic!) [cmd critcl] to rebuild the binaries.

[example {
    critcl -force foo.tcl
}]

For debugging purposes it may be handy to see the generated
intermediate [file .c] files as well. Their removal from the cache can
be prevented by specifying the option [option -keep].

[example {
    critcl -keep foo.tcl
}]

These can be combined, of course.
