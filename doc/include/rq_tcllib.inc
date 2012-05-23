
[subsection Tcllib]

To use Critcl a few packages found in the Tcllib bundle are
required. These packages are:

[list_begin enumerated]
[enum] snit
[enum] cmdline
[enum] md5
[enum] tcllibc (or Trf, which is its own package, not in Tcllib)
[list_end]

[para] Assuming that ActiveTcl is installed, or some other Tcl
installation with [syscmd teacup] available, these packages can
be installed via

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
