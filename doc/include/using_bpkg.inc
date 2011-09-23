[subsection {Building A Package}]

To build the binary package for a [file .critcl] file, instead of
[sectref {Pre-Filling The Result Cache}], simply specify the option
[option -pkg].

[example {
    critcl -pkg foo.tcl
}]

This will geneate a package named [package foo].

A simpler alternative to the above is

[example {
    critcl -pkg foo
}]

The application will automatically assume that the input file to look
for is [file foo.tcl].

[para]

But what when the name of the input file is not the name of the
package to build ? This we can handle as well:

[example {
    critcl -pkg foo bar.tcl
}]

The argument [package foo] specifies the name, and [file bar.tcl] is
the file to process.

[para]

Going back to the very first example, it is of course possible to use
an absolute path to specify the file to process:

[example {
    critcl -pkg /path/to/foo.tcl
}]

The package name derived from that is still [package foo].
