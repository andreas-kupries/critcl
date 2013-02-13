#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}
set me [file normalize [info script]]
proc main {} {
    global argv
    if {![llength $argv]} { set argv help}
    if {[catch {
	eval _$argv
    }]} usage
    exit 0
}
set packages {
    {critcl        critcl.tcl}
    {critcl-util   util.tcl}
    {critcl-class  class.tcl}
    {critcl-iassoc iassoc.tcl}
    {app-critcl   ../critcl/critcl.tcl critcl-app}
    util84
    stubs
    critcl-platform
}
proc usage {{status 1}} {
    global errorInfo
    if {[info exists errorInfo] && ($errorInfo ne {}) &&
	![string match {invalid command name "_*"*} $errorInfo]
    } {
	puts stderr $::errorInfo
	exit
    }

    global argv0
    set prefix "Usage: "
    foreach c [lsort -dict [info commands _*]] {
	set c [string range $c 1 end]
	if {[catch {
	    H${c}
	} res]} {
	    puts stderr "$prefix$argv0 $c args...\n"
	} else {
	    puts stderr "$prefix$argv0 $c $res\n"
	}
	set prefix "       "
    }
    exit $status
}
proc +x {path} {
    catch { file attributes $path -permissions ugo+x }
    return
}
proc grep {file pattern} {
    set lines [split [read [set chan [open $file r]]] \n]
    close $chan
    return [lsearch -all -inline -glob $lines $pattern]
}
proc version {file} {
    set provisions [grep $file {*package provide*}]
    #puts /$provisions/
    return [lindex $provisions 0 3]
}
proc tmpdir {} {
    package require fileutil
    set tmpraw [fileutil::tempfile critcl.]
    set tmpdir $tmpraw.[pid]
    file delete -force $tmpdir
    file mkdir $tmpdir
    file delete -force $tmpraw

    puts "Assembly in: $tmpdir"
    return $tmpdir
}
proc findlib {path} {
    while {1} {
	if {[file tail $path] eq "lib"} {
	    return $path
	}
	set new [file dirname $path]
	if {$new eq $path} break
	set path $new
    }
    return $path
}
proc id {cv vv} {
    upvar 1 $cv commit $vv version

    set commit  [exec git log -1 --pretty=format:%H]
    set version [exec git describe]

    puts "Commit:      $commit"
    puts "Version:     $version"
    return
}
proc savedoc {tmpdir} {
    puts {Collecting the documentation ...}
    file copy -force embedded/www $tmpdir/doc
    return
}
proc placedoc {tmpdir} {
    file delete -force doc
    file copy -force $tmpdir/doc doc
    return
}
proc 2website {} {
    puts {Switching to gh-pages...}
    exec 2>@ stderr >@ stdout git checkout gh-pages
    return
}
proc reminder {commit} {
    puts ""
    puts "We are in branch gh-pages now, coming from $commit"
    puts ""
    return
}
proc Hhelp {} { return "\n\tPrint this help" }
proc _help {} {
    usage 0
    return
}
proc Hrecipes {} { return "\n\tList all build commands, without details." }
proc _recipes {} {
    set r {}
    foreach c [info commands _*] {
	lappend r [string range $c 1 end]
    }
    puts [lsort -dict $r]
    return
}
proc Hdoc {} { return "\n\t(Re)Generate the embedded documentation." }
proc _doc {} {
    cd [file dirname $::me]/doc

    puts "Removing old documentation..."
    file delete -force ../embedded/man
    file delete -force ../embedded/www

    file mkdir ../embedded/man
    file mkdir ../embedded/www

    puts "Generating man pages..."
    exec 2>@ stderr >@ stdout dtplite -ext n -o ../embedded/man nroff .
    puts "Generating html..."
    exec 2>@ stderr >@ stdout dtplite        -o ../embedded/www html .

    cd  ../embedded/man
    file delete -force .idxdoc .tocdoc
    cd  ../www
    file delete -force .idxdoc .tocdoc

    return
}
proc Htextdoc {} { return "destination\n\tGenerate plain text documentation in specified directory." }
proc _textdoc {dst} {
    set destination [file normalize $dst]

    cd [file dirname $::me]/doc

    puts "Removing old text documentation at ${dst}..."
    file delete -force $destination

    file mkdir $destination

    puts "Generating pages..."
    exec 2>@ stderr >@ stdout dtplite -ext txt -o $destination text .

    cd  $destination
    file delete -force .idxdoc .tocdoc

    return
}
proc Hfigures {} { return "\n\t(Re)Generate the figures and diagrams for the documentation." }
proc _figures {} {
    cd [file dirname $::me]/doc/figures

    puts "Generating (tklib) diagrams..."
    eval [linsert [glob *.dia] 0 exec 2>@ stderr >@ stdout dia convert -t -o . png]

    return
}
proc Hrelease {} { return "\n\tGenerate a release from the current commit.\n\tAssumed to be properly tagged.\n\tLeaves checkout in the gh-pages branch, ready for commit+push" }
proc _release {} {
    # # ## ### ##### ######## #############
    # Get scratchpad to assemble the release in.
    # Get version and hash of the commit to be released.

    set tmpdir [tmpdir]
    id commit version

    savedoc $tmpdir

    # # ## ### ##### ######## #############
    puts {Generate starkit...}
    _starkit $tmpdir/critcl31.kit

    # # ## ### ##### ######## #############
    puts {Collecting starpack prefix...}
    # which we use the existing starpack for, from the gh-pages branch

    exec 2>@ stderr >@ stdout git checkout gh-pages
    file copy download/critcl31.exe $tmpdir/prefix.exe
    exec 2>@ stderr >@ stdout git checkout $commit

    # # ## ### ##### ######## #############
    puts {Generate starpack...}
    _starpack $tmpdir/prefix.exe $tmpdir/critcl31.exe
    # TODO: vacuum the thing. fix permissions if so.

    # # ## ### ##### ######## #############
    2website
    placedoc $tmpdir

    file copy -force $tmpdir/critcl31.kit download/critcl31.kit
    file copy -force $tmpdir/critcl31.exe download/critcl31.exe

    set index   [fileutil::cat index.html]
    set pattern   "\\\[commit .*\\\] \\(v\[^)\]*\\)<!-- current"
    set replacement "\[commit $commit\] (v$version)<!-- current"
    regsub $pattern $index $replacement index
    fileutil::writeFile index.html $index

    # # ## ### ##### ######## #############
    reminder $commit

    # # ## ### ##### ######## #############
    return
}
proc Hrelease-doc {} { return "\n\tUpdate the release documentation from the current commit.\n\tAssumed to be properly tagged.\n\tLeaves the checkout in the gh-pages branch, ready for commit+push" }
proc _release-doc {} {
    # # ## ### ##### ######## #############
    # Get scratchpad to assemble the release in.
    # Get version and hash of the commit to be released.

    set tmpdir [tmpdir]
    id _ commit ; # Just for the printout, we are actually not using the data.

    savedoc $tmpdir
    2website
    placedoc $tmpdir
    reminder $commit

    # # ## ### ##### ######## #############
    return
}
proc Hinstall {} { return "?destination?\n\tInstall all packages, and application.\n\tdestination = path of package directory, default \[info library\]." }
proc _install {{dst {}}} {
    global packages

    if {[llength [info level 0]] < 2} {
	set dstl [info library]
	set dsta [file dirname [file dirname [file normalize [info nameofexecutable]/___]]]
    } else {
	set dstl $dst
	set dsta [file dirname [findlib $dstl]]/bin
    }

    puts {Installing into:}
    puts \tPackages:\t$dstl
    puts \tApplication:\t$dsta

    if {[catch {
	# Create directories, might not exist.
	file mkdir $dstl
	file mkdir $dsta
	set prefix \n
	foreach item $packages {
	    # Package: /name/

	    if {[llength $item] == 3} {
		foreach {dir vfile name} $item break
	    } elseif {[llength $item] == 1} {
		set dir   $item
		set vfile {}
		set name  $item
	    } else {
		foreach {dir vfile} $item break
		set name $dir
	    }

	    if {$vfile ne {}} {
		set version  [version [file dirname $::me]/lib/$dir/$vfile]
	    } else {
		set version {}
	    }

	    file copy   -force [file dirname $::me]/lib/$dir     $dstl/${name}-new
	    file delete -force $dstl/$name$version
	    file rename        $dstl/${name}-new     $dstl/$name$version
	    puts "${prefix}Installed package:      $dstl/$name$version"
	    set prefix {}
	}

	# Application: critcl

	set    c [open $dsta/critcl w]
	puts  $c "#!/bin/sh\n# -*- tcl -*- \\\nexec [file dirname [file normalize [info nameofexecutable]/___]] \"\$0\" \$\{1+\"\$@\"\}\npackage require critcl::app\ncritcl::app::main \$argv"
	close $c
	+x $dsta/critcl

	puts "${prefix}Installed application:  $dsta/critcl"
    } msg]} {
	if {![string match {*permission denied*} $msg]} {
	    return -code error -errorcode $::errorCode -errorinfo $::errorInfo $msg
	}
	puts stderr "\n$msg\n\nUse 'sudo' or some other way of running the operation under the user having access to the destination paths.\n"
	exit
    }
    return
}
proc Hdrop {} { return "?destination?\n\tRemove packages.\n\tdestination = path of package directory, default \[info library\]." }
proc _drop {{dst {}}} {
    global packages

    if {[llength [info level 0]] < 2} {
	set dstl [info library]
	set dsta [file dirname [file dirname [file normalize [info nameofexecutable]/___]]]
    } else {
	set dstl $dst
	set dsta [file dirname $dst]/bin
    }

    foreach item $packages {
	# Package: /name/

	if {[llength $item] == 3} {
	    foreach {dir vfile name} $item break
	} elseif {[llength $item] == 1} {
	    set dir   $item
	    set vfile {}
	    set name  $item
	} else {
	    foreach {dir vfile} $item break
	    set name $dir
	}

	if {$vfile ne {}} {
	    set version  [version [file dirname $::me]/lib/$dir/$vfile]
	} else {
	    set version {}
	}

	file delete -force $dstl/$name$version
	puts "Removed package:     $dstl/$name$version"
    }

    # Application: critcl

    file delete $dsta/critcl
    puts "Removed application: $dsta/critcl"
    return
}
proc Hstarkit {} { return "?destination? ?interpreter?\n\tGenerate a starkit\n\tdestination = path of result file, default 'critcl.kit'\n\tinterpreter = (path) name of tcl shell to use for execution, default 'tclkit'" }
proc _starkit {{dst critcl.kit} {interp tclkit}} {
    package require vfs::mk4

    set c [open $dst w]
    fconfigure $c -translation binary -encoding binary
    puts -nonewline $c "#!/bin/sh\n# -*- tcl -*- \\\nexec $interp \"\$0\" \$\{1+\"\$@\"\}\npackage require starkit\nstarkit::header mk4 -readonly\n\032################################################################################################################################################################"
    close $c

    vfs::mk4::Mount $dst /KIT
    file copy -force lib /KIT
    file copy -force main.tcl /KIT
    vfs::unmount /KIT
    +x $dst

    puts "Created starkit: $dst"
    return
}
proc Hstarpack {} { return "prefix ?destination?\n\tGenerate a fully-selfcontained executable, i.e. a starpack\n\tprefix      = path of tclkit/basekit runtime to use\n\tdestination = path of result file, default 'critcl'" }
proc _starpack {prefix {dst critcl}} {
    package require vfs::mk4

    file copy -force $prefix $dst

    vfs::mk4::Mount $dst /KIT
    file mkdir /KIT/lib

    foreach d [glob -directory lib *] {
	file delete -force  /KIT/lib/[file tail $d]
	file copy -force $d /KIT/lib
    }

    file copy -force main.tcl /KIT
    vfs::unmount /KIT
    +x $dst

    puts "Created starpack: $dst"
    return
}
proc Hexamples {} { return "?args...?\n\tWithout arguments, list the examples.\n\tOtherwise run the recipe with its arguments on the examples." }
proc _examples {args} {
    global me
    set selfdir [file dirname $me]
    set self    [file tail    $me]

    # List examples, or run the build code on the examples, passing any arguments.

    set examples [glob -directory $selfdir/examples */$self]

    puts ""
    if {![llength $args]} {
	foreach b $examples {
	    puts "* [file dirname $b]"
	}
    } else {
	foreach b $examples {
	    puts "$b _______________________________________________"
	    eval [linsert $args 0 exec 2>@ stderr >@ stdout [info nameofexecutable] $b]
	    puts ""
	    puts ""
	}
    }
    return
}
main
