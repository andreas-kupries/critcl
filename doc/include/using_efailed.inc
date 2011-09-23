[section {Checking your C}]

As said several times, by default critcl defers the compile and link
steps for a file until it is needed, i.e. the first command of the
[file .critcl] file in question is actually invoked.

[para] This not only has the effect of lazily loading the package's
functionality, but also, when developing using mode "compile & run",
of us not seeing any errors in our code until we are actually trying
to run some demonstration.

[para] If we do not wish to have such a delay we have to be able to
force at least the execution of the compile step.

[para] The command [cmd critcl::failed] is exactly that. When called
it forcibly builds the C code for the [file .critcl] file it is part
of, and returns a boolean vlaue signaling failure ([const true]), or
success ([const false]).

[example {
    package require critcl

    ... Declare C procedures, commands, etc.

    if {[critcl::failed]} {
        ... signal error
    }
}]

It is related and similar to [cmd critcl::load], the command to
overcome the lazy loading, as shown in section
[sectref {Unlazy Packages}].

[para] Like it is not allowed, nor possible to use critcl commands
declaring anything after [cmd critcl::failed] has been called, making
it pretty much the last critcl command in a [file .critcl] file.

Code like

[example {
    package require critcl

    ... Declare C procedures, commands, etc.

    if {[critcl::failed]} { ... }

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
