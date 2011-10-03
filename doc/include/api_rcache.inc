
This package provides two commands for the management of the
[sectref {Result Cache}]. See that section for background information.

[para] [emph NOTE] that these commands are irrelevant to anybody just
wishing to write a package using critcl for the C parts. They are
management commands which are only useful to the
[manpage {CriTcl Application}] or similar tools.


[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::cache] [opt path]]

This command sets and returns the path to the directory for the
package's result cache.

[para] The default location is
[file ~/.critcl/[lb]platform::generic[rb]] and usually does not
require any changes.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::clean_cache] [opt [arg pattern]...]]

This command cleans the result cache, i.e. removes any and all files
and directories in it. If one or more patterns are specified then only
the files and directories matching them are removed.

[list_end]
