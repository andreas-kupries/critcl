
[subsection Tcllib]

To use Critcl a few packages found in the Tcllib bundle are
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
the CVS repository for the two bundles can be found at
[uri https://sourceforge.net/projects/tcllib].

[para] Releases of Tcllib can be found in the associated Files
section, or CVS can be used to check out the sub-project tcllib.

[para] Tcllib comes with its own installation instructions. These will
not be repeated here. If there are problems with its directions please
file a bug against the Tcllib project at the above url, and not
Critcl.
