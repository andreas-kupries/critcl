[section {Changes for version 3.1.14}]
[list_begin enumerated]

[enum]  Fixed issue #45. New feature command [cmd critcl::cconst].

[enum]  [package critcl::util]: New command [cmd locate] to find a
        file across a set of paths, and report an error when not
        found. This is for use in autoconf-like header-searches and
        similar configuration tests.

[enum]  Modified 'AbortWhenCalledAfterBuild' to dump the entire stack
        (info frame!). This should make it easier to determine the
        location of the troubling declaration.

[list_end]
