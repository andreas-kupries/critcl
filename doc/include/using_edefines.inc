[subsection {Lifting constants}]

When writing a critcl-based package to make a third-party library
available to scripts we do not only have to make the relevant
functions available as commands, often we also have to know all
the various constants, flags, etc. these functions take.

[para] Rather than writing such magic numbers directly we would
greatly prefer to use symbolic names instead. 

Instead of providing one or more commands to list and map the magic
numbers to strings critcl [emph only] provides a single command which
allows the export of C defines and enumeration values, mapping them to
Tcl variables of the given names, whose values are the associated
magic numbers.

[para] This is good enough because the developers of the third-party
library were very likely like us and wanted to use symbolic names
instead of magic numbers. Which in C are declared as via defines and
enumeration types. We just have to [emph lift] them up.

[para] Our example comes from [package cryptkit], a Tcl binding to
[package cryptlib], a cryptography library.

The command

[example {
    critcl::cdefines CRYPT_* ::crypt
}]

maps all Cryptlib specific #defines and enums into the namespace
[namespace ::crypt], telling critcl to create aliases to the symbols.

[para] Similarly

[example {
    critcl::cdefines {
        NULL
        TRUE
        FALSE
        TCL_OK
        TCL_ERROR
    } ::crypt
}]

maps the listed defines into the namespace [namespace ::crypt].

[para] An [emph important] thing to note:

These commands [emph {do not create}] the defines in the C level. They
only lift pre-existing material.

Which can come from the headers of the third-party library, the usual
case, but also from [sectref {Blocks of arbitrary C}].

[para] A corrollary to the above: What is not where, cannot be
lifted. All listed names and patterns which have no actual C code
declaring them are ignored, i.e. not mapped.
