[section {Changes for version 3.1}]
[list_begin enumerated]

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Extended the API for handling TEApot metadata. Added the
command [cmd critcl::meta?] to query the stored information. Main use
currently envisioned is retrieval of the current package's name by
utility command, for use in constructed names. This particular
information is always available due to the static scan of the package
file on execution of the first critcl command.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Extended the API with a command, [cmd critcl::name2c] exposing
the process of converting a Tcl name into base name, namespace, and C
namespace.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Extended the API with a command, [cmd critcl::source],
executing critcl commands in a separate file in the context of the
current file, allowing for easier management of larger bodies of code
by allowing the user to split it up in easier to digest chunks without
causing the generation of multiple packages.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Related to the previous item, extended the API with commands to
divert code collection into memory, making the use of commands for
embedded C code by higher level code generators easier.

[para] See the section [sectref {Advanced: Diversions}] for details of
the provided commands, and the package [package critcl::class] for a
user of these facilities.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Extended the API with commands helping developers of
higher-level packages generating their own C code to deal with the
embedding of proper C [term #line] directives.

[para] See the section [sectref {Advanced: Location management}] for
details of the provided commands, and the packages
[package critcl::iassoc] and [package critcl::class] for users of
these facilities.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Added the higher-level package [package critcl::iassoc].

[para] This package simplifies the creation of code associating data
with an interpreter via Tcl's [fun Tcl_(Get|Set)AssocData()] APIs. The
user can concentrate on his data while all the neccessary boilerplate
to support it is generated/handled by the package.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Added the higher-level package [package critcl::class]. 

[para] This package simplifies the creation of C level objects with
class and instance commands. The user sees a class definition with
class- and instance-variables and -methods similar to a TclOO class,
with all the necessary boilerplate to support them generated/handled
by the package.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[enum] Extended the API with commands for the definition of custom
argument and result types for use by [cmd ::critcl::cproc].

See the section [sectref {Advanced: Extending cproc}] for details of
the provided commands.

[comment {- - -- --- ----- -------- ------------- ---------------------}]
[list_end]
