#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}
package require Tcl 8.4
unset -nocomplain ::errorInfo
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
    {app-critcl       {.. critcl critcl.tcl} critcl-app}
    {critcl           critcl.tcl}
    {critcl-bitmap    bitmap.tcl}
    {critcl-class     class.tcl}
    {critcl-cutil     cutil.tcl}
    {critcl-emap      emap.tcl}
    {critcl-enum      enum.tcl}
    {critcl-iassoc    iassoc.tcl}
    {critcl-literals  literals.tcl}
    {critcl-platform  platform.tcl}
    {critcl-util      util.tcl}
    {critcl-objtype   objtype.tcl}
    {dict84           dict.tcl}
    {lassign84        lassign.tcl}
    {lmap84           lmap.tcl}
    {stubs_container  container.tcl}
    {stubs_gen_decl   gen_decl.tcl}
    {stubs_gen_header gen_header.tcl}
    {stubs_gen_init   gen_init.tcl}
    {stubs_gen_lib    gen_lib.tcl}
    {stubs_gen_macro  gen_macro.tcl}
    {stubs_gen_slot   gen_slot.tcl}
    {stubs_genframe   genframe.tcl}
    {stubs_reader     reader.tcl}
    {stubs_writer     writer.tcl}
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
proc critapp {dst} {
    global tcl_platform
    set app [file join $dst critcl]
    if {$tcl_platform(platform) eq "windows"} {
	append app .tcl
    }
    return $app
}
proc vfile {dir vfile} {
    global me
    set selfdir [file dirname $me]
    eval [linsert $vfile 0 file join $selfdir lib $dir]
}
proc grep {file pattern} {
    set lines [split [read [set chan [open $file r]]] \n]
    close $chan
    return [lsearch -all -inline -glob $lines $pattern]
}
proc version {file} {
    set provisions [grep $file {set version__*}]
    if {$provisions ne {}} {return [lindex $provisions 0 2] }
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
proc dstlfromlib {path} {
    # kinda the inverse of findlib above, it returns the path to
    # dstl relative the */lib path returned by findlib. The path
    # is returned as a list of segments
    set relpath {}
    while {1} {
        if {[file tail $path] eq "lib"} {
            break
        }
        set new [file dirname $path]
        set relpath [linsert $relpath[set relpath {}] 0 [file tail $path]]
        if {$new eq $path} break
        set path $new
    }
    return $relpath
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
    file copy -force [file join embedded www] [file join $tmpdir doc]
    return
}
proc pkgdirname {name version} {
	return $name$version
}
proc placedoc {tmpdir} {
    file delete -force doc
    file copy -force [file join $tmpdir doc] doc
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
proc shquote value {
    return "\"[string map [list \\ \\\\ $ \\$ ` \\`] $value]\""
}
proc targets libdir {
    if {$libdir eq {} } {
	set exe  [file dirname [file normalize [file join [info nameofexecutable] ...]]]
	set dstl [info library]
	set dsta [file dirname $exe]
	set dsti [file join [file dirname $dsta] include]
    } else {
	set dstl $libdir
	set libdir [findlib $dstl]
	set top [file dirname $libdir]
	set dsta [file join $top bin]
	set dsti [file join $top include]
    }
    list $dsta $dsti $dstl
}
proc query {q c} {
    puts -nonewline "$q ? "
    flush stdout
    set a [string tolower [gets stdin]]
    if {($a ne "y" ) && ($a ne "yes")} {
	puts "$c"
	exit 1
    }
}

proc special {pkg pdirbase srcdir cdir {hdir {}}} {
    # Import context
    upvar 1 dstl dstl dsti dsti selfdir selfdir target target theapp theapp prefix prefix

    puts "\nInstalled C package:\t$pkg"

    # Compute destinations and other information
    set src     [glob -dir [file join $selfdir lib $srcdir] *.tcl]
    set version [version $src]
    set dst     [file join $dstl [pkgdirname $pdirbase $version]]
    set dsth    [file join $dsti $pdirbase]

    # Assemble the critcl command to build the package, and run it.
    set cmd     {}
    lappend cmd exec >@ stdout 2>@ stderr
    lappend cmd [info nameofexecutable]
    lappend cmd $theapp
    if {$target ne {}} { lappend cmd -target $target }
    set dstl_tmp [file join $dstl tmp]
    lappend cmd -libdir     $dstl_tmp
    lappend cmd -includedir $dstl_tmp
    lappend cmd -pkg $src

    puts [list executing $cmd]
    eval $cmd

    # Process build results, place into install destinations.
    file delete -force $dst $dsth
    file rename  [file join $dstl_tmp $cdir] $dst
    if {$hdir ne {}} {
	file rename [file join $dstl_tmp $hdir] $dsth
    }
    file delete -force $dstl_tmp

    puts "${prefix}Installed package:      $dst"
    if {$hdir ne {}} {
	puts "${prefix}Installed headers:      [file join $dsti $hdir]"
    }
    return
}

proc Hsynopsis {} { return "\n\tGenerate a synopsis of procs and builtin types" }
proc _synopsis {} {
    puts Public:
    puts [exec grep -n ^proc lib/critcl/critcl.tcl \
	      | sed -e "s| \{$||" -e {s/:proc ::critcl::/ /} \
	      | grep -v { [A-Z]} \
	      | grep -v { at::[A-Z]} \
	      | sort -k 2 \
	      | sed -e {s/^/    /}]

    puts Private:
    puts [exec grep -n ^proc lib/critcl/critcl.tcl \
	      | sed -e "s| \{$||" -e {s/:proc ::critcl::/ /} \
	      | grep {[A-Z]} \
	      | sort -k 2 \
	      | sed -e {s/^/    /}]

    puts "Builtin argument types:"
    puts [exec grep -n {    argtype} lib/critcl/critcl.tcl \
	      | sed -e "s| \{$||" -e {s/:[ 	]*argtype/ /} \
	      | sort -k 2 \
	      | sed -e {s/^/    /}]

    puts "Builtin result types:"
    puts [exec grep -n {    resulttype} lib/critcl/critcl.tcl \
	      | sed -e "s| \{$||" -e {s/:[ 	]*resulttype/ /} \
	      | sort -k 2 \
	      | sed -e {s/^/    /}]

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
proc Htest {} { return "\n\tRun the testsuite." }
proc _test {} {
    global argv
    set    argv {} ;# clear -- tcltest shall see nothing
    # Run all .test files in the test directory.
    set selfdir [file dirname $::me]
    foreach testsuite [lsort -dict [glob -directory [file join $selfdir test] *.test]] {
	puts ""
	puts "_ _ __ ___ _____ ________ _____________ _____________________ *** [file tail $testsuite] ***"
	if {[catch {
	    exec >@ stdout 2>@ stderr [info nameofexecutable] $testsuite
	}]} {
	    puts $::errorInfo
	}
    }

    puts ""
    puts "_ _ __ ___ _____ ________ _____________ _____________________"
    puts ""
    return
}
proc Hdoc {} { return "\n\t(Re)Generate the embedded documentation." }
proc _doc {} {
    cd [file join [file dirname $::me] doc]

    puts "Removing old documentation..."
    file delete -force [file join .. embedded man]
    file delete -force [file join .. embedded www]

    file mkdir [file join .. embedded man]
    file mkdir [file join .. embedded www]

    puts "Generating man pages..."
    exec 2>@ stderr >@ stdout dtplite -ext n -o [file join .. embedded man] nroff .
    puts "Generating html..."
    exec 2>@ stderr >@ stdout dtplite        -o [file join .. embedded www] html .

    cd  [file join .. embedded man]
    file delete -force .idxdoc .tocdoc
    cd  [file join .. www]
    file delete -force .idxdoc .tocdoc

    return
}
proc Htextdoc {} { return "destination\n\tGenerate plain text documentation in specified directory." }
proc _textdoc {dst} {
    set destination [file normalize $dst]

    cd [file join [file dirname $::me] doc]

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
    cd [file join [file dirname $::me] doc figures]

    puts "Generating (tklib) diagrams..."
    eval [linsert [glob *.dia] 0 exec 2>@ stderr >@ stdout dia convert -t -o . png]

    return
}
proc Hrelease {} { return "\n\tGenerate a release from the current commit.\n\tAssumed to be properly tagged.\n\tLeaves checkout in the gh-pages branch, ready for commit+push" }
proc _release {} {
    # # ## ### ##### ######## #############
    # Get scratchpad to assemble the release in.
    # Get version and hash of the commit to be released.

    query "Have you run the tests"              "Please do"
    query "Have you run the examples"           "Please do"
    query "Have you bumped the version numbers" "Came back after doing so!"

    set tmpdir [tmpdir]
    id commit version

    savedoc $tmpdir

    # # ## ### ##### ######## #############
    #puts {Generate starkit...}
    #_starkit [file join $tmpdir critcl31.kit]

    # # ## ### ##### ######## #############
    #puts {Collecting starpack prefix...}
    # which we use the existing starpack for, from the gh-pages branch

    #exec 2>@ stderr >@ stdout git checkout gh-pages
    #file copy [file join download critcl31.exe] [file join $tmpdir prefix.exe]
    #exec 2>@ stderr >@ stdout git checkout $commit

    # # ## ### ##### ######## #############
    #puts {Generate starpack...}
    #_starpack [file join $tmpdir prefix.exe] [file join $tmpdir critcl31.exe]
    # TODO: vacuum the thing. fix permissions if so.

    # # ## ### ##### ######## #############
    2website
    placedoc $tmpdir

    #file copy -force [file join $tmpdir critcl31.kit] [file join download critcl31.kit]
    #file copy -force [file join $tmpdir critcl31.exe] [file join download critcl31.exe]

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

proc Htargets {} { return "?destination?\n\tShow available targets.\n\tExpects critcl app to be installed in destination." }
proc _targets args {
    switch [llength $args] {
	0 - 1 {
	}
	default {
	    error -list wrong # args
	}
    }
    if {[llength [info level 0]] < 2} {
	lassign [targets {}] dsta dsti dstl
    } else {
	lassign [targets [file join [file dirname [lindex [info level 0] 1]] lib]] dsta dsti dstl
    }
    puts [join [split [exec [file join $dsta critcl] -targets]] \n]
    return
}

proc Hinstall {} { return "?-target T? ?destination?\n\tInstall all packages, and application.\n\tdestination = path of package directory, default \[info library\]." }
proc _install {args} {
    global packages me

    set target {}
    if {[lindex $args 0] eq "-target"} {
	set target [lindex $args 1]
	set args [lrange $args 2 end]
    }

    if {[llength $args] == 0} {
	set libdir {}

    } else {
	set libdir [lindex $args 0]
    }
    lassign [targets $libdir] dsta dsti dstl
    file mkdir $dsta $dsti

    set selfdir [file dirname $me]

    puts {Installing into:}
    puts \tPackages:\t$dstl
    puts \tApplication:\t$dsta
    puts \tHeaders:\t$dsti

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
		set version [version [vfile $dir $vfile]]
	    } else {
		set version {}
	    }

	    set namevers [file join $dstl [pkgdirname $name $version]]

	    file copy -force [file join $selfdir lib $dir] [file join $dstl ${name}-new]
	    file delete -force $namevers
	    puts "${prefix}Installed package:      $namevers"
	    file rename [file join $dstl ${name}-new] $namevers
	    set prefix {}
	}

	# Application: critcl

	set theapp  [critapp     $dsta]
	set reldstl [dstlfromlib $dstl]

	set c [open $theapp w]
	lappend map @bs@   "\\"
	lappend map @exe@ [shquote [file dirname [file normalize [
	    file join [info nameofexecutable] ...]]]]
	lappend map @path@ [list $reldstl]  ;# insert the dst path
	puts [list geedonk $reldstl]
	lappend map "\t    " {} ;# de-dent
	puts $c [string trimleft [string map $map {
	    #!/bin/sh
	    # -*-tcl -*-
	    # hide next line from tcl @bs@
	    exec @exe@ "$0" ${1+"$@"}

	    set libpath [file join [file dirname [file dirname [
		file normalize [file join [info script] ...]]]] .. lib]
	    set libpath [file join $libpath @path@]
	    if {[lsearch -exact $auto_path $libpath] < 0} {
		set auto_path [linsert $auto_path[set auto_path {}] 0 $libpath]
	    }

	    package require critcl::app
	    critcl::app::main $argv}]]
	close $c
	+x $theapp

	puts "${prefix}Installed application:  $theapp"

	# Special packages:
	# - critcl_md5c		Local MD5 hash implementation.
	# - critcl::callback	C/Tcl callback utility code.
	# - critcl::objtrack	C/Tcl Tcl_Obj* tracking (Support leak investigations)
	# Note: All compilation uses package mode, which does not use MD5.
	# Thus there is no chicken vs egg problem with the first special.

	special critcl::md5c     critcl_md5c     critcl-md5c     md5c
	special critcl::callback critcl_callback critcl-callback callback critcl_callback
	special critcl::objtrack critcl_objtrack critcl-objtrack objtrack critcl_objtrack

    } msg]} {
	if {![string match {*permission denied*} $msg]} {
	    return -code error -errorcode $::errorCode -errorinfo $::errorInfo $msg
	}
	puts stderr "\n$msg\n\nUse 'sudo' or some other way of running the operation under the user having access to the destination paths.\n"
	exit
    }
    return
}
proc Huninstall {} { Hdrop }
proc _uninstall {args} { eval [linsert $args 0 _drop] }

proc Hdrop {} { return "?destination?\n\tRemove packages.\n\tdestination = path of package directory, default \[info library\]." }
proc _drop {{dst {}}} {
    global packages me

    if {[llength [info level 0]] < 2} {
	set dstl [info library]
	set dsta [file dirname [file dirname [file normalize [file join [
	    info nameofexecutable] ...]]]]
    } else {
	set dstl $dst
	set dsta [file join [file dirname $dst] bin]
    }

    # Add the special package (see install). Not special with regard
    # to removal. Except for the name
    lappend packages [list critcl-md5c md5c.tcl critcl_md5c]

    set selfdir [file dirname $me]

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
	    set version [version [vfile $dir $vfile]]
	} else {
	    set version {}
	}

	set namevers [file join $dstl $name$version]

	file delete -force $namevers
	puts "Removed package:     $namevers"
    }

    # Application: critcl
    set theapp [critapp $dsta]
    file delete $theapp
    puts "Removed application: $theapp"
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
    file mkdir [file join /KIT lib]

    foreach d [glob -directory lib *] {
	file delete -force  [file join /KIT lib [file tail $d]]
	file copy -force $d [file join /KIT lib]
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

    set examples [lsort -dict [glob -directory [file join $selfdir examples] */$self]]

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
