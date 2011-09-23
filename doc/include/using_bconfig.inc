[subsection {Custom Configurations}]

The compiler configurations coming with [package critcl] currently
cover all hosts having [syscmd gcc] installed (the foremost among
these are Linux and OS X), plus the native compilers of the more
common unix-like operating systems, i.e. Solaris, HP-UX, and AIX, and,
on the non-unix side, Windows.

[para]

Developers using operating systems and compilers outside of this range
will either have to install a [syscmd gcc]-based development
environment, i.e. get into the covered range, or write their own
custom configuration and then tell [cmd critcl] about it.

[para]

The latter is the easier part, given that [cmd critcl] supports the
option [option -config] whose argument is the path to the file
containing the custom configuration(s). I.e.

[example {
    critcl -config /path/to/config ...
}]

will run [cmd critcl] with the custom configuration in
[file /path/to/config], with the other options and arguments as
explained in previous sections. Depending on the choice of name for
the new configuration(s) this may or may not require a
[option -target] option to select the configuration needed.

[para]

For the former, the writing of the custom configuration, the reader is
refered to the section "Configuration Internals" of the
[manpage {CriTcl Package Reference}] for the necessary details.

This is an advanced topic pretty much out of scope for this tutorial
beyond what was already said.
