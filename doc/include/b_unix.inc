[subsection {Build & Installation (Unix)}]

This section describes the actions required to install CriTcl on Unix
systems (Linux, BSD, and related, including OS X).

If you have to install CriTcl on a Windows machine see section
[sectref {Build & Installation (Windows)}] instead.

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

Note that the installed critcl application is modified to use the
chosen tclsh instead of searching for one on the [var PATH].

[para]

On Windows you can invoke the file [file build.tcl] with a
double-click.  This will pop up a small graphical interface for
entering the destination and performing the installation. This
handling of a double-click is restricted to Windows only however.

[para]

The build system provides a small GUI for those not comfortable with
the command line.

This GUI is accessible by invoking [file build.tcl] without any
arguments.

[para]

To get help about the methods of [file build.tcl], and their complete
syntax, invoke  [file build.tcl] with argument [method help], i.e., like

[example {
    /path/to/tclsh /path/to/critcl/build.tcl help
}]
