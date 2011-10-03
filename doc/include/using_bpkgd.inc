[subsection {Building For Debugging}]

Here we assume that you know the basics of how to build a package.
If not, please read section [sectref {Building A Package}] first.

[para]

An important issue, when there is trouble with the package, debugging
becomes necessary a evil.

Critcl supports this through the [option -debug] option.
Using it enables various build modes which help with that.

[para]

For example, to activate the Tcl core's built-in memory debugging
subsystem build your package with

[example {
    critcl -pkg -debug memory foo
}]

The resulting binary for package [package foo] will use Tcl's
debug-enabled (de)allocation functions, making them visible to Tcl's
[cmd memory] command. This of course assumes that the Tcl core used
was also built for memory debugging.

[para]

Further, built your package with

[example {
    critcl -pkg -debug symbols foo
}]

to see the [package foo]'s symbols (types, functions, variables, etc.)
when inspecting a [file core] file it is involved in with a symbolic
debugger,

[para]

To activate both memory debugging and symbols use either

[example {
    critcl -pkg -debug all foo
}]

or

[example {
    critcl -pkg -debug symbols -debug memory foo
}]
