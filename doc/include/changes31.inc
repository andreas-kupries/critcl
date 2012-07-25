[section {Changes for version 3.1}]
[list_begin enumerated]

[enum] Extended the API for handling TEApot metadata. Added the
command [cmd critcl::meta?] to query the stored information. Main use
currently envisioned is retrieval of the current package's name by
utility command, for use in constructed names. This particular
information is always available due to the static scan of the package
file on execution of the first critcl command.

[enum] Extended the API with a command, [cmd critcl::name2c] exposing
the process of converting a Tcl name into base name, namespace, and C
namespace.

[enum] Extended the API with a command, [cmd critcl::source] executing
critcl commands in a separate file in the context of the current file,
allowing for easier management of larger bodies of code by splitting
it up.

[enum] Extended the API with commands helping developers of
higher-level packages generating their own C code to deal with the
embedding of proper C [term #line] directives. See the section
[sectref {Advanced: Location management}] for details of the provided
commands, and the packages [package critcl::iassoc] and
[package critcl::class] for users of these facilities.

[list_end]
