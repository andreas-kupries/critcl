[subsection {First Use, Testing the Installation}]

With critcl installed it is now the time to try at least one of the
examples distributed with it. This will also test if the installation
was successful.

[para] Below I show the steps to generate and then use the low- and
high-level stack example packages. I am intentionally bypassing the
[file build.tcl] file the example is coming with, to show the use of
[cmd critcl] itself.

[para] Some more explanations before running the example:

[list_begin itemized]

[item]  Here [file path/to/critcl] is the path to the installed
	critcl application, not the critcl source directory.

[para]	Also, on Windows(tm) this part of the example must be
	replaced with

	[example { /path/to/tclsh /path/to/critcl }]

	as the installed application has no extension and
	thus Windows will not know how to execute the script.

[para]	Only if the installed application was manually renamed to
	[file critcl.tcl] and the machine has an association for
	[file .tcl] to either [syscmd tclsh] or [syscmd wish] then
	the [file /path/to/tclsh] can be left out.

[item]	The example shows only the commands entered on the shell (and
	tclsh) command line. Their responses are left out.

[para]	If any command throws an error, i.e. has a problem, then all
	following commands will run into some other error as well,
	as a consequence of the first problem.

[item]	Use of option [option -keep] causes critcl to leave the
	generated .c files behind, for edification. Normally this
	happens only in case of trouble.

[item]	Use of option [option -cache] redirects the location of the
	directory to hold generated and build files to a local
	directory with a known name, for an easy look after.

[item]	Both [package cstack] and [package stackc] have to use the
	[emph same] [option -cache] so that [package stackc] will
	find the stub table headers exported by [package cstack].
[list_end]

[para][example {

    > cd examples/stack
    > /path/to/critcl -keep -cache B -pkg cstack.tcl
    > /path/to/critcl -keep -cache B -pkg stackc.tcl

    > tclsh
    % lappend auto_path [pwd]/lib
    % package require stackc
    % join [info loaded] \n
    % stackc create S
    % S push FOO
    % S size
    % S destroy
    % exit
    >
}]
