[subsection {Making A Widget}]

... requires compiling against the Tk headers, and linking with Tk's
stubs. For our convenience we have a simple, single command to
activate all the necessary machinery, with critcl supplying the header
files and stubs C code, instead of having to make it work on our own
via [cmd critcl::cflags], [cmd critcl::ldflags],
[cmd critcl::cheaders], [cmd critcl::csources].

[para] This command is [cmd critcl::tk].

[example {
    package require critcl
    critcl::tk ; # And now critcl knows to put in the Tk headers and other support.

    ... Declare your code ...
}]

Please note that this doesn't release you from the necessity of
learning Tk's C API and how to use it to make a widget work. Sorry.
