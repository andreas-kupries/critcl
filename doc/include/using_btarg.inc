[subsection {Retargeting The Binaries}]

The configuration settings [cmd critcl] uses to drive the compiler,
linker, etc. are by default selected based on the platform it is run
on, to generate binaries which properly work on this platform.

[para]

There is one main use-case for overriding this selection, which is
done with the option [option -target]:

[list_begin enumerated]
[enum]
Cross-compilation. The building of binaries for a platform T
while critcl actually runs on platform B.

The standard configuration of [cmd critcl] currently has settings for
two cross-compilation targets. So, to build 32bit Windows binaries on
a Linux host which has the Xmingw cross-compilation development
environment installed use

[example {
    critcl -pkg -target mingw32 foo
}]

Similarly, building a package for use on ARM processors while critcl
is running in an Intel environment use

[example {
    critcl -pkg -target linux-arm foo
}]

Note that both configurations assume that the cross-compiling
compiler, linke, etc. are found first in the [var PATH].

[list_end]

[comment {

First, to choose among several possible C compiler available on the
build host. The standard configuration for most unix platforms uses
the platform's native compiler, for example, and


}]
