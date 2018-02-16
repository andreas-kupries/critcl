
The package provides six commands to control compilation and linking.
These are:

[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::check] [opt [arg label]] [arg text]]

This command is useful to test if some functionality is available in
the build environment, and then select other C code fragments based on
that information.

It immediately compiles the C code in [arg text] and returns a boolean
value based on the result of the compilation.

The command returns [const true] on success, and [const false] otherwise.

If specified, the [arg label] is used to uniquely mark the check in the
generated log.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::checklink] [opt [arg label]] [arg text]]

This command is an extenson of [cmd critcl::check] above, useful to
test if some functionality is available in the build environment, and
then select other C code fragments based on that information.

It immediately compiles and links the C code in [arg text] and returns
a boolean value based on the result of compilation and linking.

The command returns [const true] on success, and [const false] otherwise.

If specified, the [arg label] is used to uniquely mark the check in the
generated log.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::msg] [opt [option -nonewline]] [arg msg]]

This command can be used by critc-based code to report results from
[cmd critcl::check] and [cmd critcl::checklink]. The default
implementation used by mode [term {compile & run}] ignores any calls.

[para] Tools like the [manpage {CriTcl Application}] are allowed to
redefine this procedure to perform their own way of message
reporting. The package [package critcl::app] and the application on
top print such messages to [term stdout], for example.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::print] [opt [option -nonewline]] [opt [arg chan]] [arg msg]]

This command is used by the critcl internals to report its activity.
Its signature is equivalent to the Tcl builtin command [cmd ::puts].
The default implementation is effectively [cmd ::puts].

[para] Tools directly using either the critcl package, or the critcl
application package are allowed to redefine this procedure to perform
their own way of printing.

[para] An example of this is
[uri https://chiselapp.com/user/andreas_kupries/repository/Kettle/index Kettle]
where the newest revisions use this to highlight build warnings.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::compiled]]

This command returns a boolean value. It returns [const true] if the C
code of the current [file .critcl] file is already compiled, and
[const false] otherwise.

[para] This predicate effectively enables a [file .critcl] file used
as its own Tcl companion file (see [cmd critcl::tsources]) to
distinguish between sourced by mode "compile & run" for compilation
and sourced from either the result of mode "generate package" or during
the load phase of "compile & run".

In case of the two latter possibilities the result is [const true],
and [const false] for the first.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::compiling]]

This command returns a boolean value. It returns [const true] if C
code can be compiled on this platform in general, i.e. if a C compiler
is available, and [const false] otherwise.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::done]]

This command returns a boolean value. It returns [const true] when
critcl has built the embedded C code, and [const false] otherwise.

[para] This enables the Tcl code of a critcl-based package to
distinguish between it getting used as a prebuilt package, versus
dynamic compile & run, and take action based on that.

[para] [emph Note] that this command is only useful from within a
[file .critcl] file. The result is managed on a per-file basis, like
is done for the commands embedding C code and controlling the
behaviour of compiler and linker.

[para] See also section [sectref {Modes Of Operation/Use}].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::failed]]

This command returns a boolean value. It returns [const true] if
critcl has failed to build the package, and [const false] otherwise,
i.e. success. As part of this it forces the building of the package,
but not its loading. Note that it will attempt to build the package
only on the first call; future calls for the same package will return
a cached result.

[para] This enables a critcl-based package to check itself for
availability and throw an error if it could not be built. [emph Note]
that the command does [emph not] throw such an error itself.

[para] [emph Note] further that this command is only useful from
within in a [file .critcl] file. The result is managed on a per-file
basis, like is done for the commands embedding C code and controlling
the behaviour of compiler and linker.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::load]]

This command is like [cmd critcl::failed], except that it also forces
the loading of the generated shared library, if it was built, and that
its result has reversed sense. 

[para] It returns [const true] if critcl succeeded in building and
loading the package, and [const false] otherwise, i.e. build- or
load-failure.

[para] This enables a critcl-based package to to not only check itself
for availability and throw an error if it could not be built, but also
force an immediate load, circumventing the default behaviour, which is
lazy. See also section [sectref {Runtime Behaviour}]. [emph Note] that
the command does [emph not] throw any error itself.

[para] [emph Note] further that this command is only useful from
within in a [file .critcl] file. The result is managed on a per-file
basis, like is done for the commands embedding C code and controlling
the behaviour of compiler and linker.

[list_end]
