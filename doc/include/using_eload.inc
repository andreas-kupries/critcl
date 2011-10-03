[subsection {Unlazy Packages}]

By default critcl is a bit inconsistent between modes "compile & run"
and "generate package". The result of the latter is a standard Tcl
package which loads and sources all of its files immediately when it
is required. Whereas "compile & run" defers actual compilation,
linking, and loading until the first time one of the declared commands
is actually used, making this very lazy.

[para] This behaviour can be quite unwanted if Tcl companion files, or
other users of the C commands use introspection to determine the
features they have available. Just using [lb][cmd {info commands}][rb]
doesn't cut it, the [var auto_index] array has to be checked as well,
making things quite inconvenient for the users.

[para] To fix this issue at the source, instead of in each user, be it
inside of the package itself, or other packages, we have the command
[cmd critcl::load].

Used as the last command in a [file .critcl] file it forces the
compile, link, and load trinity, ensuring that all C commands are
available immediately.

[example {
    package require critcl

    ... Declare C procedures, commands, etc.

    critcl::load ; # Force build and loading.
}]

Note that is not allowed, nor possible to use critcl commands
declaring anything after [cmd critcl::load] has been called. I.e., code like

[example {
    package require critcl

    ... Declare C procedures, commands, etc.

    critcl::load ; # Force build and loading.

    ... More declarations of C code, ...
    critcl::code { ... }
}]

will result in an error. The only package-related commands still
allowed are

[list_begin enumerated]
[enum] [cmd critcl::done]
[enum] [cmd critcl::failed]
[enum] [cmd critcl::load]
[list_end]

as these only query information, namely the build status, and are
protected against multiple calls.
