
[subsection Tcllib]

To use CriTcl a few packages found in the Tcllib bundle are
required. These packages are:

[list_begin enumerated]
[enum] [package cmdline]
[enum] [package md5].
[para] And to accelerate this package, it is recommend to get and
install one of
[list_begin enumerated]
[enum] [package tcllibc]
[enum] [package md5c]
[enum] [package Trf]
[list_end]
The system will work without them, but can become quite slow,
especially when handling large code blocks.
[enum] [package snit]
[list_end]

[para] Assuming that ActiveTcl is installed, or some other Tcl
installation with [syscmd teacup] available, most (not md5c) of these
packages can be installed via

[example {
    teacup install $packagename
}]

[para] The [syscmd teacup] command above assumes that ActiveState's
TEApot repository at [uri http://teapot.activestate.com] is in the
list of repositories accessible to [syscmd teacup]. This is automatically
ensured for the ActiveTcl distribution. Others may have to run

[example {
    teacup archive add http://teapot.activestate.com
}]

to make this happen.

[para] Now, for those wishing to install the packages from source,
the fossil repository for the two bundles can be found at
[uri https://core.tcl.tk/tcllib] and [uri https://core.tcl.tk/tklib].

[para] Releases of Tcllib and Tklib can be found there as well, or
[syscmd fossil] can be used to check out specific revisions.

[para] Tcl- and Tklib come with their own installation instructions.
These will not be repeated here. If there are problems with their
directions please file a bug against the Tcllib project at the above
url, and not CriTcl.
