
This package provides four commands for the management of the
build configuration, i.e. the per-platform information about
compilers, linkers, and their commandline options.

[para] [emph NOTE] that these commands are irrelevant to anybody just
wishing to write a package using critcl for the C parts. They are
management commands which are only useful to the
[manpage {CriTcl Application}] or similar tools.

[list_begin definitions]

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::readconfig] [arg path]]

This command reads the build configuration file at [arg path] and
configures the package using the information for the currently set
target platform.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::showconfig] [opt [arg chan]]]

This command converts the currently active build configuration into a
human-readable string and prints the result to the channel [arg chan].

If [arg chan] is not present the string is instead returned as the
result of the command.

[comment {
    As a package it would be IMHO better to have these command only
    return the necessary internal data, and have the application
    higher up do the formatting.
}]


[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::showallconfig] [opt [arg chan]]]

This command converts the set of all known build configurations (from
the currently active build configuration file last set with
[cmd critcl::readconfig]) into a string and print the result to the
channel [arg chan].

If [arg chan] is not present the string is instead returned as the
result of the command.


[comment {
    As a package it would be IMHO better to have these command only
    return the necessary internal data, and have the application
    higher up do the formatting.
}]


[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::chooseconfig] [arg target] [opt [arg nomatcherr]]]

This command takes a target identifier and matches it against all
known targets, returning a list containing all the matching ones. This
search is first done on an exact basis, and then via glob matching. If
no known target matches the argument the default is to return an empty
list. However, if the boolean [arg nomatcherr] is specified and set,
and error will be thrown instead, using [cmd critcl::error].


[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::setconfig] [arg target]]

This command takes a target identifier and configures the package to
use all its settings.

[list_end]
