
[subsection Tcl]

As we are building a Tcl package that should be pretty much obvious
that a working Tcl installation is needed, and I will not belabor the
point.

[para] Out of the many use whatever you are comfortable with, as long
as it provides Tcl 8.5, or higher, and Tk 8.5 or higher.

[para] This may a Tcl installation provided by your operating system
distribution, from a distribution-independent vendor, or built by
yurself.

[para] Myself, I used [uri http://www.activestate.com ActiveState's]
ActiveTcl 8.5 distribution during development of the binding, as I am
most familiar with it.

[para] [emph {(Disclosure: I, Andreas Kupries, work for ActiveState, maintaining ActiveTcl and TclDevKit for them).}]

[para] This distribution can be found at
[uri http://www.activestate.com/activetcl]. Retrieve the archive of
ActiveTcl 8.5 for your platform and install it as directed by
ActiveState.

[para] Assuming that ActiveTcl got installed I usually run the command

[example {
    teacup update
}]

to install all packages ActiveState provides, and the kitchensink, as
the distribution itself usually contains only the ost important set of
packages. This ensures that the dependencies for CriTcl are all
present, and more.

[para] If that is not to your liking you have to read the sections for
CriTcl to determine the exact set of packages required, and install
only these using

[example {
    teacup install $packagename
}]

[para] Both [syscmd teacup] commands above assume that ActiveState's
TEApot repository at [uri http://teapot.activestate.com] is in the
list of repositories accessible to [syscmd teacup]. This is automatically
ensured for the ActiveTcl distribution. Others may have to run

[example {
    teacup archive add http://teapot.activestate.com
}]

to make this happen.

[para] For those wishing to build Tcl/Tk on their own, their sources
can be found at

[list_begin definitions]
[def Tcl]   [uri http://core.tcl.tk/tcl/]
[def Tk]    [uri http://core.tcl.tk/tk/]
[list_end]
