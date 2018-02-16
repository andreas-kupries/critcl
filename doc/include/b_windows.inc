[subsection {Build & Installation (Windows)}]

This section describes the actions required to install CriTcl on
Windows(tm) systems.

If you have to install CriTcl on a Unix machine (Linux, BSD, and
related, including OS X) see section
[sectref {Build & Installation (Unix)}] instead.

To install Critcl simply run

[example {
    /path/to/tclsh /path/to/critcl/build.tcl install
}]

where [file /path/to/tclsh] is the tclsh of your Tcl installation, and
[file /path/to/critcl] the location of the Critcl sources on your system.

[para]

This builds all packages and then places them in a directory where the
[cmd tclsh] will find them. 

[para]

It further creates a [file critcl] application script and places it
into the directory [cmd tclsh] resides in, making it a sibling of that
executable. 

[para][emph Attention!] Note that while the installed critcl
application is modified to use the chosen tclsh instead of searching
for one on the [var PATH] this is useless for Windows, which
associates executables with files through their extension.

[para][emph Attention!] The current installer does not put an
extension on the [syscmd critcl] application, forcing users to either
explicitly choose the [syscmd tclsh] to run the application, or
manually rename the installed file to [file critcl.tcl], if an
association for [file .tcl] is available, to either [syscmd tclsh], or
[syscmd wish].

[para]

On Windows you can invoke the file [file build.tcl] with a
double-click.  This will pop up a small graphical interface for
entering the destination and performing the installation. This
handling of a double-click is restricted to Windows only however.

[para]

This GUI is also accessible by invoking [file build.tcl] without any
arguments.

[para]

To get help about the methods of [file build.tcl], and their complete
syntax, invoke  [file build.tcl] with argument [method help], i.e., like

[example {
    /path/to/tclsh /path/to/critcl/build.tcl help
}]
