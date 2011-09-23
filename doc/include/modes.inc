
CriTcl can be used in two different modes of operation, called

[list_begin enumerated]
[enum] [term {Compile & Run}], and
[enum] [term {Generate Package}]
[list_end]

Of these two [term {Compile & Run}] came first and is the default when
using the package directly. In that case the package collects the C
fragments, builds them as needed, and caches the results for quick
reuse when the same code is used in the future again.

[para]

The second mode, [term {Generate Package}], was introduced to enable
the creation of (prebuilt) deliverable packages which do not depend on
the existence of a build system, i.e. C compiler, on the target
machine.

This was originally done through the experimental [cmd Critbind] tool,
and is now handled by the [manpage {CriTcl Application}], also named
[cmd critcl].
