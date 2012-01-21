[section {Changes for version 3.1}]
[list_begin enumerated]

[enum] Extended the API for handling TEApot metadata. Added the
command [cmd meta?] to query the stored information. Main use
currently envisioned is retrieval of the current package's name by
utility command, for use in constructed names. This particular
information is always available due to the static scan of the package
file on execution of the first critcl command.

[enum] Extended the API with a command, [cmd name2c] exposing the
process of converting a Tcl name into base name, namespace, and C
namespace.

[list_end]
