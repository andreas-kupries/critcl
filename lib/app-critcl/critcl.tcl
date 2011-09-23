#!/bin/sh
# -*- tcl -*-
# # ## ### ##### ######## ############# #####################

# Critcl Application.

# # ## ### ##### ######## ############# #####################

#   Prebuild shared libraries using the Critcl package.
#
#   Based originally on critbind by Jean-Claude Wippler
#   Transmogrified into critcl   by Steve Landers
#
#   $Id: critcl.tcl 4717 2009-11-02 01:24:42Z stevel $
#
# \
    exec tclkit $0 ${1+"$@"}

# # ## ### ##### ######## ############# #####################
## Requirements

package provide critcl::app [package require critcl]
package require cmdline
package require platform

# Note: We can assume here that the commands lassign and dict are
# available. The critcl package has made sure of that.

namespace eval ::critcl::app {}

# # ## ### ##### ######## ############# #####################

# Intercept 'package' calls.

# This code is present to handle the possibility of building multiple
# different versions of the same package, or of different packages
# having dependencies on different versions of a 3rd party
# package. Each will 'package provide' its version to our Tcl, and
# thus normally be reported as a conflict. To prevent that the
# intercepted command checks for this situation, and forces Tcl to
# forget the previously registered package.

rename package _package
proc package {option args} {
    if {$option eq "provide"} {
        if {![catch {
	    set v [_package present [lindex $args 0]]
	}] &&
	    ([llength $args] > 1) &&
	    ($v ne [lindex $args 1])
	} {
	    # A package is provided which is already present in
	    # memory, the number of arguments is ok, and the version
	    # of the new package is different from what is
	    # known. Force Tcl to forget the previous package, this is
	    # not truly a conflict.
            _package forget [lindex $args 0]
        }
    }

    return [eval [linsert $args 0 _package $option]]
}

# # ## ### ##### ######## ############# #####################
## Override the default of the critcl package for errors and
## message. Write them to the terminal (and, for errors, abort the
## application instead of throwing them up the stack to an uncertain
## catch).

proc ::critcl::error {msg} {
    global argv0
    puts stderr "$argv0 error: $msg"
    exit 1
}

proc ::critcl::msg {args} {
    switch -exact -- [llength $args] {
	1 {
	    puts stdout [lindex $args 0]
	}
	2 {
	    lassign $args o m
	    if {$o ne "-nonewline"} {
		return -code error "wrong\#args, expected: ?-nonewline? msg"
	    }
	    puts -nonewline stdout $m
	}
	default {
	    return -code error "wrong\#args, expected: ?-nonewline? msg"
	}
    }
    return
}

# # ## ### ##### ######## ############# #####################

proc ::critcl::app::main {argv} {
    Cmdline $argv

    # When creating a package use a transient cache which is not in
    # conflict with "compile & run", or other instances of the critcl
    # application.

    if {$v::pkg} {
	set pkgcache [PackageCache]
	critcl::cache $pkgcache
    }

    ProcessInput
    StopOnFailed

    # All input files have been processed and their data saved. Now
    # generate the boilerplate bracketing all the sub-ordinate
    # Foo_Init() functions, i.e. the code which provides a single
    # initialization function for the whole set of input files.

    if {$v::pkg} {
	# Create a merged shared library and put a proper Tcl package
	# around it.

	BuildBracket
	StopOnFailed
	AssemblePackage

	if {$v::pkg && !$v::keep} {
	    file delete -force $pkgcache
	}
    }

    StopOnFailed

    if {$v::keep} {
	puts stderr "Files left in [critcl::cache]"
    }
    return
}

proc ::critcl::app::PackageCache {} {
    if {$v::cache ne {}} {
	return $v::cache
    }
    return [file join ~ .critcl pkg[pid].[clock seconds]]
}

proc ::critcl::app::StopOnFailed {} {
    if {!$v::failed} return
    puts stderr "Files left in [critcl::cache]"
    puts stderr "FAILURES $v::failed"
    puts stderr "FAILED:  [join $v::borken "\nFAILED:  "]"
    puts stderr "FAILED   [join [split [join $v::log \n\n] \n] "\nFAILED   "]"
    return -code return
}

proc ::critcl::app::Cmdline {argv} {
    variable options

    # Rationalized application name. Direct user is the intercepted
    # '::critcl::error' command.
    set ::argv0 [cmdline::getArgv0]

    # Semi-global application configuration.
    set v::verbose  0  ; # Default, no logging.
    set v::src     {}  ; # No files to process.
    set v::pkg      0  ; # Fill cache. When set build a package.
    set v::shlname ""  ; # Name of shlib to build.
    set v::outname ""  ; # Name of shlib dir to create.
    set v::libdir  lib ; # Directory to put the -pkg directory into.
    set v::keep    0   ; # Default: Do not keep generated .c files.

    # Local actions.
    set selftest 0 ;# Invoke the application selftest, which simply
                    # runs whatever test/*.tst files are found in the
                    # starkit or starpack. IOW, this functionality is
                    # usable only for a wrapped critcl application.
    set cleaning 0 ;# Clean the critcl cache. Default: no.
    set showall  0 ;# Show all configurations in full. Default: no.
    set show     0 ;# Show the chosen build configuration. Default: no.
    set targets  0 ;# Show the available targets.
    set help     0 ;# Show the application's help text.

    # Local configuration. Seen outside of this procedure only
    # directly, through the chosen build configuration.

    set target     "" ;# The user-specified build target, if any.
    set configfile "" ;# The user-specified custom configuration file, if any

    # Process the command line...

    while {[set result [cmdline::getopt argv $options opt arg]] != 0} {
	if {$result == -1} {
	    Usage "Unknown option \"$opt\""
	}
	switch -exact -- $opt {
	    I          { critcl::config I $arg }
	    cache      { set v::cache $arg }
	    clean      { incr cleaning }
	    config     { set configfile $arg }
	    debug      {
		lappend v::debug $arg
		critcl::config lines 0
	    }
	    force      {
		critcl::config force 1
		puts stderr "Compilation forced"
	    }
	    keep       {
		critcl::config keepsrc 1
		critcl::config lines 0
		set v::keep 1
	    }
	    help       { incr help }
	    libdir     { set v::libdir $arg }
	    pkg        { incr v::pkg ; incr v::verbose }
	    show       { incr show }
	    showall    { incr showall }
	    target     { set target $arg }
	    targets    { incr targets }
	    test       { set selftest 1 }
	    default {
		Usage "Unknown option \"$opt\""
	    }
	}
    }

    # ... validate the settings, and act on them.

    if {$help} {
	Help
	exit
    }

    # Parse the user-specified configuration file, if any. This
    # overrides the default configuration file read by the critcl
    # package when it was loaded. It does keep the default platform
    # from that active.

    if {$configfile ne ""} {
	if {$argv eq "" && [file extension $configfile] eq ".tcl"} {
	    # probably means the user has omitted the config file and we've
	    # picked up the source file name
	    Usage "-config is missing file argument"
	}
	if {![file exists $configfile]} {
	    Usate "Can't read configuration file $configfile"
	}
	critcl::readconfig $configfile
    }

    # And switch to the user-provided target platform.

    if {$target ne ""} {
	if {($argv eq "") && [file extension $target] eq ".tcl"} {
	    # probably means the user has omitted the config file and we've
	    # picked up the source file name
	    Usage "-target is missing file argument"
	}

	set match [critcl::chooseconfig $target 1]

	if {[llength $match] == 1} {
	    critcl::setconfig [lindex $match 0]
	} else {
	    Usage "multiple targets matched : $match"
	}
    }

    if {$v::pkg || $show} {
	critcl::crosscheck
    }

    if {$cleaning} {
	critcl::clean_cache
    }

    if {$show} {
	critcl::showconfig stdout
    }

    if {$showall} {
	critcl::showallconfig stdout
    }

    if {$targets} {
	puts [critcl::knowntargets]
    }

    if {$show || $showall || $targets} {
	exit
    }

    if {$selftest} {
	Selftest
	exit
    }

    # Invoking the application without input files is an error, except
    # if it was to just clean the local critcl cache.

    if {[llength $argv] < 1} {
	if {!$cleaning} Usage
	exit
    }

    # The remainder of the arguments are the files to process, except
    # for lib and pkg modes where they can be prefixed with the name
    # of the output file, i.e. shared library. If however no such
    # output file is present the name of the first input file will be
    # used as name of the library.

    set v::src $argv

    # (%) Determine the name of the shared library to generate from
    # the input files. This location is referenced by (=).

    if {$v::pkg} {
	set name [lindex $argv 0]

	# Split a version number off the package name.
	set ver {}
	if {[regexp {^([^0-9]+)([0-9][.0-9]*)$} $name -> base ver]} {
	    set name $base
	}

	switch [file extension $name] {
	    .dll   -
	    .dylib -
	    .sl    -
	    .so {
		# The name of the result shlib is prefixed, take it as
		# package name, and strip it off the list of input
		# files.
		set v::outname [file rootname $name]
		set v::src     [lrange $v::src 1 end]
	    }
	    .tcl {
		# We have no discernible result shlib, take
		# the stem of the first input file as package
		# name

		set v::outname [file rootname $name]
	    }
	    "" {
		# See above for .tcl, except that there is no stem to
		# take. And if this is the only argument we also have
		# to derive the full name of the expected input file
		# from it.
		set v::outname $name
		if {[llength $argv] == 1} {
		    set v::src [list $v::outname.tcl]
		} else {
		    set v::src [lrange $v::src 1 end]
		}
	    }
	    default {
		Usage "Not sure how to handle \"$name\""
	    }
	}

	# Put the version number back. We have to distinguish package
	# library file name and package directory name. Only the
	# latter should have the version number.
	set v::shlname $v::outname
	if {$ver ne {}} {
	    append v::outname $ver
	}

	if {[file extension $v::shlname] eq ""} {
	    append v::shlname [critcl::sharedlibext]
	}
	critcl::config combine dynamic

	if {![llength $v::src]} {
	    Usage "No input files"
	}
    }

    # Determine the platform to use by the build backend, based on
    # actual platform we are on and the user's chosen target, if any.

    set v::actualplatform [::critcl::actualtarget]
    return
}

proc ::critcl::app::Log {text} {
    if {!$v::verbose} return
    puts -nonewline $text
    flush stdout
    return
}

proc ::critcl::app::LogLn {text} {
    if {!$v::verbose} return
    puts $text
    flush stdout
    return
}

proc ::critcl::app::Usage {args} {
    global argv0
    if {[llength $args]} {
	puts stderr "$argv0 error: [join $args]"
    }

    puts stderr [string map [list @ $argv0] {To compile and run a tcl script
	@ [-force] [-keep] [-cache dir] file[.tcl]

To compile and build a package
    @ options -pkg ?name? [files...]

Options include:
    -debug [symbols|memory|all] enable debugging
    -force          force compilation of C files
    -show           show the configuration options being used
    -target target  generate binary for specified target platform/architecture

Other options that may be useful:
    -I dir          adds dir to the include path when compiling.
    -cache dir      sets the Critcl cache directory to dir.
    -keep           keep intermediate C files in the Critcl cache
    -config file    read the Critcl configuration options from file
    -libdir dir     location of generated library/package
    -showall        show configuration for all supported platforms
    -targets        show all available target platforms

You can display the built-in help wiki on most platforms using:
    @ -help }]
    exit 1
    return
}

proc ::critcl::app::Help {} {
    if {[catch {package require Mk4tcl} msg] ||
	[catch {package require Wikit} msg]} {
	puts $msg
        set txt "Couldn't load the Critcl help Wiki\n"
        append txt "To display the Critcl help wiki run \"critcl\" "
        append txt "without any options.\n"
        puts $txt
        exit
    } else {
        Wikit::init [file join $::starkit::topdir doc critcl.tkd]
    }
}

proc ::critcl::app::Selftest {} {
    foreach t [glob -directory [file join $starkit::topdir test] *.tst] {
        source $t
    }
    return
}

proc ::critcl::app::ProcessInput {} {
    # Main loop. This processes the input files, one by one.

    set v::debug [lsort -unique $v::debug]

    # NOTE that this effectively executes them (source!) in the
    # context of this application. The files are trusted to not
    # contain malicious side-effects, etc.

    # Initialize the accumulator variables for various per-file
    # information which will be needed later when building the
    # over-arching initialization code.

    set v::clibraries {}  ;# External libraries used. To link the final shlib against.
    set v::ldflags    {}  ;# Linker flags.
    set v::objects    {}  ;# The object files to link.
    set v::edecls     {}  ;# Initialization function decls for the pieces.
    set v::initnames  {}  ;# Initialization function calls for the pieces.
    set v::tsources   {}  ;# Tcl companion sources.
    set v::mintcl     8.4 ;# Minimum version of Tcl required to run the package.
    set v::tk         0   ;# Boolean flag. Set if any sub-package needs Tk, forcing it on the collection as well.
    set v::preload    {}  ;# List of libraries declared for preload.
    set v::license    {}  ;# Accumulated licenses, if any.
    set v::failed      0  ;# Number of build failures encountered.
    set v::borken     {}  ;# List of files which failed to build.
    set v::log        {}  ;# List of log messages for the failed files
    set v::pkgs       {}  ;# List of package names for the pieces.
    set v::inits      {}  ;# Init function names for the pieces, list.

    # Other loop status information.

    set first  1  ;# Flag, reset after first round, helps with output formatting.
    set missing 0

    if {[llength $v::src]} {
	LogLn "Config:   [::critcl::targetconfig]"
	LogLn "Build:    [::critcl::buildplatform]"

	set t [::critcl::targetplatform]
	if {$v::actualplatform ne $t} {
	    LogLn "Target:   $v::actualplatform (by $t)"
	} else {
	    LogLn "Target:   $v::actualplatform"
	}
	Log   "Source:   "
    }

    foreach f $v::src {
	# Avoid reloading itself.
	if {[file rootname [file tail $f]] eq "critcl"} continue

	# Canonicalize input argument, and search in a few places.
	set f [file normalize $f]

	set found [file exists $f]
	if {!$found} {
	    if {[file extension $f] ne ".tcl"} {
		append f .tcl
		set found [file exists $f]
	    }
	    if {!$found} {
		if {!$first} { puts stderr "" }
		puts stderr "$f doesn't exist"
		incr missing
		continue
	    }
	}

	set first 0
	Log "[file tail $f] "
	set dir [file dirname $f]

	# Execute the input file and collect all the crit(i)c(a)l :)
	# information. Depending on the use of 'critcl::failed' this
	# may or may not have generated the internal object file.

	if {$v::pkg} {
	    critcl::buildforpackage
	}

	if {[llength $v::debug]} {
	    # As the debug settings are stored per file we now take
	    # the information from the application's commandline and
	    # force things here, faking the proper path information.

	    set save [info script]
	    info script $f
	    eval [linsert $v::debug 0 critcl::debug ]
	    info script $save
	}

	# Ensure that critcl's namespace introspection is done
	# correctly, and not thinking that 'critcl::app' is the
	# namespace to use for the user's commands.

	uplevel #0 [list source $f]

	if {[critcl::cnothingtodo $f]} {
	    puts stderr "nothing to build for $f"
	    continue
	}

	# Force build. Our 'buildforpackage' call above disabled
	# 'critcl::failed' and 'critcl::load' (Causing them to return
	# OK, and bypassing anything conditional on their failure). If
	# there is a failure we want to know it correctly, here.
	#
	# Regardless, we have to force (and later restore) the proper
	# script location, something the 'source' comand above did
	# automatically.

	set save [info script]
	info script $f
	set failed [critcl::cbuild $f 0]
	incr v::failed $failed
	info script $save

	# We can skip the part where we collect the build results for
	# use by the overarching code if either no overall shlib is
	# generated from the input, or any of the builds made so far
	# failed.

	# NOTE that we were NOT skipping the build step for any of the
	# packages, even if previous packages failed. We want the
	# maximum information about problems from a single run, not
	# fix things one by one.

	if {$failed} {
	    lappend v::borken $f
	    lappend v::log    [dict get [critcl::cresults $f] log]
	    Log "(FAILED) "
	}
	if {$v::failed || !$v::pkg} continue

	array set r [critcl::cresults $f]

	append v::edecls    "extern Tcl_AppInitProc $r(initname)_Init;\n"
	append v::initnames "    if ($r(initname)_Init(ip) != TCL_OK) return TCL_ERROR;\n"
	append v::license   [License $f $r(license)]

	lappend v::pkgs  $r(pkgname)
	lappend v::inits $r(initname)

	# The overall minimum version of Tcl required by the combined
	# packages is the maximum over all of their minima.
	set v::mintcl [Vmax $v::mintcl $r(mintcl)]
	set v::tk     [Max $v::tk $r(tk)]
	critcl::lappendlist v::objects    $r(objects)
	critcl::lappendlist v::tsources   $r(tsources)
	critcl::lappendlist v::clibraries $r(clibraries)
	critcl::lappendlist v::ldflags    $r(ldflags)
	critcl::lappendlist v::preload    $r(preload)
    }

    if {$missing} {
	critcl::error  "Missing files: $missing, aborting"
    }

    # Reduce package and init function to the first pieces. Easier to
    # do it this way than having a conditional set in the loop.

    set v::pkgs  [lindex $v::pkgs  0]
    set v::inits [lindex $v::inits 0]
    # Strip the prefix used by the foundation package. Keep in sync.
    regsub {^ns_} $v::inits {} v::inits

    return
}

proc ::critcl::app::Vmax {a b} {
    if {[package vcompare $a $b] >= 0} {
	return $a
    } else {
	return $b
    }
}

proc ::critcl::app::Max {a b} {
    if {$a >= $b} {
	return $a
    } else {
	return $b
    }
}

proc ::critcl::app::License {file text} {
    if {$text eq "<<Undefined>>"} { return {} }
    return "\n\[\[ [file tail $file] \]\] __________________\n$text"
}

proc ::critcl::app::BuildBracket {} {
    puts "\nLibrary:  [file tail $v::shlname]"

    # The overarching initialization code, the bracket, has no real
    # file behind it. Fake it based on the destination shlib, this
    # ensures that the generated _Init function has the proper name
    # without having to redefine things through C macros, as was done
    # before.
    info script $v::shlname

    critcl::config combine ""

    # Inject the information collected from the input files, making
    # them part of the final result.
    critcl::tcl $v::mintcl
    if {$v::tk} { critcl::tk }

    set                 lib critcl::cobjects
    critcl::lappendlist lib $v::objects
    eval $lib

    set                 lib critcl::clibraries
    critcl::lappendlist lib [lsort -unique $v::clibraries]
    eval $lib

    eval [linsert [lsort -unique $v::ldflags] 0 critcl::ldflags]
    eval [linsert [lsort -unique $v::preload] 0 critcl::preload]

    critcl::cinit $v::initnames $v::edecls

    # And build everything.
    critcl::buildforpackage 0
    set failed [critcl::cbuild "" 0]

    incr v::failed $failed
    if {$failed} {
	lappend v::borken <<Bracket>>
	Log "(FAILED) "
    }
    return
}

proc ::critcl::app::PlaceShlib {} {
    # Copy the generated shlib from the cache to its final resting
    # place. For -lib that is wherever the user specified, wheras for
    # -pkg this was set be inside the directory hierarchy of the
    # newly-minted package. To prevent hassle a previously existing
    # file gets deleted.

    if {[file exists $v::shlname]} {
	file delete -force $v::shlname
    }

    # NOTE that the fake 'info script location' set by 'BuildBracket'
    # is still in effect, making access to the build results easy.
    set shlib [dict get [critcl::cresults] shlib]
    file copy $shlib $v::shlname

    # For MSVC debug builds we get a separate debug info file.
    set pdb [file root $shlib].pdb
    if {[file exists $pdb]} {
	file copy -force $pdb [file root $v::shlname].pdb
    }

    return
}

proc ::critcl::app::AssemblePackage {} {

    # Validate and/or create the main destination directory L. The
    # package will become a subdirectory of L. See (x). And a platform
    # specific directory inside of that will hold the shared
    # library. This allows us to later merge the packages for
    # different platforms into a single multi-platform package.

    set libdir [file normalize $v::libdir]
    if {[file isfile $libdir]} {
	critcl::error "can't package $v::shlname - $libdir is not a directory"
    } elseif {![file isdirectory $libdir]} {
	file mkdir $libdir
    }

    set libname  [file rootname [file tail $v::outname]]
    set pkgdir   [file join $libdir $libname]
    set shlibdir [file join $pkgdir $v::actualplatform]

    # XXX fileutil::stripPwd ...
    if {[string first [pwd] $pkgdir] != -1} {
	set first [string length [pwd]]
	set dir [string range $pkgdir [incr first] end]
    } else {
	set dir $pkgdir
    }
    puts "Package:  $dir"

    file mkdir             $pkgdir
    file mkdir             $shlibdir

    set shl [file tail $v::shlname]

    CreatePackageIndex     $shlibdir [file root $shl] \
	[PlaceTclCompanionFiles $pkgdir]
    CreateLicenseTerms     $pkgdir
    CreateRuntimeSupport   $pkgdir

    # At last, place the shlib generated by BuildBracket into its
    # final resting place, in the directory hierarchy of the
    # just-assembled package.

    set v::shlname [file join $shlibdir $shl]
    PlaceShlib
    return
}

proc ::critcl::app::CreatePackageIndex {shlibdir libname tsources} {
    # Build pkgIndex.tcl

    # XXX Consider doing more here than defering to the runtime.
    # XXX See ticket (38bf01b26e).

    set pkgdir  [file dirname $shlibdir]
    set map     [Mapping]
    set preload [Preload $shlibdir]

    # (=) This works because (a) 'ProcessInput' sources the package
    # files in its own context, this process, and (b) the package
    # files (are expected to) contain the proper 'package provide'
    # commands (for compile & run mode), and we expect that at least
    # one of the input files specifies the overall package built from
    # all the inputs. See also (%) in Cmdline, where the application
    # determines shlib name and package name, often from the first
    # input file, and/or working backwards from package name to input
    # file.

    set version [package present $v::pkgs]

    set    index [open [file join $pkgdir pkgIndex.tcl] w]
    puts  $index [::critcl::app::PackageGuard $v::mintcl]
    puts  $index {source [file join $dir critcl-rt.tcl]}
    puts  $index "::critcl::runtime::loadlib \$dir [list $v::pkgs $version $libname $v::inits $tsources $map] $preload"
    close $index
    return
}

proc ::critcl::app::Mapping {} {
    # Create the platform mapping for each of the platforms listed on
    # the Config platform line

    set map    [critcl::getconfigvalue platform]
    set minver [lindex $map 1]

    set plats  [list]
    foreach plat [lrange $map 2 end] {
	set mapping($plat) [list [critcl::actualtarget] $minver]
	lappend plats $plat
    }

    if {[llength $plats]} {
	puts "Platform: [join $plats {, }] $minver and later"
    }

    set map {}
    foreach plat [lsort [array names mapping]] {
	lappend map $plat $mapping($plat)
    }
    return $map
}

proc ::critcl::app::Preload {shlibdir} {
    if {![llength $v::preload]} { return {} }

    # Locate the external libraries declared for preloading and put
    # them into the package. Put the shared library of the internal
    # preload support pseudo-package into it as well. This will all be
    # picked up by the 'package ifneeded' script.

    # First handle the declared libraries. Any problem there throws an
    # error, or aborts.

    set preload {}
    foreach shlib $v::preload {
	file copy -force [PreloadLocation $shlib] $shlibdir
	lappend preload [file tail $shlib]
    }

    # Everything was ok, now place the supporting shlib into the
    # package as well.

    file copy -force \
	[file join [critcl::cache] preload[critcl::sharedlibext]] \
	$shlibdir

    puts "Preload:  [join $preload {, }]"
    return $preload
}

proc ::critcl::app::PreloadLocation {shlib} {
    set searchpath [PreloadSearchPath $shlib]

    foreach path $searchpath {
	if {![file exists $path]} continue
	return $path
    }

    set    msg "can't find preload library $shlib"
    append msg " for target platform \"$v::actualplatform\";"
    append msg " searched for "
    append msg [linsert [join $searchpath {, }] end-1 and]
    critcl::error $msg
    return
}

proc ::critcl::app::PreloadSearchPath {shlib} {

    # Look for lib FOO as follows:
    # (1) FOO.so
    # (2) FOO/FOO.so
    # (3) FOO/<platform>/FOO.so
    #
    # Look for lib BAR/FOO as follows:
    # (1) FOO.so
    #
    # Then, if BAR/FOO doesn't exist as directory:
    # (2) BAR/FOO.so
    # (3) BAR/<platform>/FOO.so
    #
    # Conversely, if BAR/FOO does exist as directory:
    # (2) BAR/FOO/FOO.so
    # (3) BAR/FOO/<platform>/FOO.so

    #   - lib.so
    #   - dir/lib.so
    #   - dir/plat/lib.so

    set tail [file tail $shlib]

    if {[file isdirectory $shlib]} {
	set dir $shlib
    } else {
	set dir [file dirname $shlib]
	if {$dir eq "."} {
	    set dir $tail
	}
    }

    set ext [critcl::sharedlibext]	    
    return [list \
		$tail$ext \
		[file join $dir $tail$ext] \
		[file join $dir $v::actualplatform $tail$ext]]
}

proc ::critcl::app::PackageGuard {v} {
    return [string map [list @ $v] \
	{if {![package vsatisfies [package provide Tcl] @]} {return}}]
}

proc ::critcl::app::CreateLicenseTerms {pkgdir} {
    # Create a license.terms file.

    if {$v::license eq ""} {
	set v::license <<Undefined>>
    } else {
	set v::license [string trimleft $v::license]
    }
    set    license [open [file join $pkgdir license.terms] w]
    puts  $license $v::license
    close $license
    return
}

proc ::critcl::app::PlaceTclCompanionFiles {pkgdir} {
    # Arrange for the companion Tcl source files (as specified by
    # critcl::tsources) to be copied into the Tcl subdirectory (in
    # accordance with TIP 55)

    if {![llength $v::tsources]} { return {} }

    set tcldir [file join $pkgdir tcl]
    file mkdir $tcldir
    set files {}
    foreach t $v::tsources {
	file copy -force $t $tcldir
	lappend files [file tail $t]
    }
    return $files
}

proc ::critcl::app::CreateRuntimeSupport {pkgdir} {
    # Create the critcl-rt.tcl file in the generated package. This
    # provides the code which dynamically assembles at runtime the
    # package loading code, i.e. the 'package ifneeded' command
    # expected by Tcl package management.

    variable mydir
    set runtime [file join $mydir runtime.tcl]

    if {![file exists $runtime]} {
	critcl::error "can't find Critcl's package runtime support file \"runtime.tcl\""
    }

    set fd [open $runtime]
    set txt [read $fd]
    close $fd

    append txt [DummyCritclPackage]
    append txt [PlatformGeneric]

    set    fd [open [file join $pkgdir critcl-rt.tcl] w]
    puts  $fd $txt
    close $fd
    return
}

proc ::critcl::app::DummyCritclPackage {} {
    # This command provides conditional no-ops for any of the critcl
    # procedures exported by the regular package, so that a .tcl file
    # with embedded C can also be its own companion file declaring Tcl
    # procedures etc. These dummy procedures are defined if and only
    # if their regular counterpart is not present.

    # Note: We are generating code checking each and every relevant
    # command individually to avoid trouble with different versions of
    # critcl which may export a differing set of procedures. This way
    # we will not miss anything just because we assumed that the
    # presence of critcl::FOO also implies having critcl::BAR, or not.

    # Append dummy Critcl procs
    # XXX This should be made conditional on the .tcl actually using itself as companion.
    append txt "\n\# Dummy implementation of the critcl package, if not present\n"

    foreach name [lsort [namespace eval ::critcl {namespace export}]] {
	# XXX BUG?: compiled is alias of compiling, should return the same ?!
	# XXX Use an array ?
	switch $name {
	    compiled  { set result 1 }
	    compiling { set result 0 }
	    done      { set result 1 }
	    check     { set result 0 }
	    failed    { set result 0 }
	    load      { set result 1 }
	    default   { set result {}}
	}
	append txt [DummyCritclCommand $name $result]
    }

    return $txt
}

proc ::critcl::app::DummyCritclCommand {name result} {
    if {$result ne {}} {
	set result "return $result"
    }

    append txt "if \{!\[llength \[info commands ::critcl::$name\]\]\} \{\n"
    append txt "    namespace eval ::critcl \{\}\n"
    append txt "    proc ::critcl::$name \{args\} \{$result\}\n"
    append txt "\}\n"
    return $txt
}

proc ::critcl::app::PlatformGeneric {} {
    # Return a clone of the platform::generic command, from the
    # currently loaded platform package. The generated package cannot
    # assume that the deployment environment contains this package. To
    # avoid trouble if the DP has the package the definition is made
    # conditional, i.e. the clone is skipped if the command is already
    # present.

    set body [info body ::platform::generic]

    append txt "\n# Define a clone of platform::generic, if needed\n"
    append txt "if \{!\[llength \[info commands ::platform::generic\]\]\} \{\n"
    append txt "    namespace eval ::platform \{\}\n"
    append txt "    proc ::platform::generic \{\} \{"
    append txt [join [split $body \n] "\n    "]
    append txt "\}\n"
    append txt "\}\n\n"

    return $txt
}


# # ## ### ##### ######## ############# #####################

namespace eval ::critcl::app {
    # Path of the application package directory.
    variable myself [file normalize [info script]]
    variable mydir [file dirname $myself]

    variable options {
	I.arg cache.arg clean config.arg debug.arg force help
	keep libdir.arg pkg show showall target.arg targets
	test
    }

    # Application state
    namespace eval v {
	# - -- --- ----- -------- ------------- ---------------------
	# Data collected from the command line.

	variable verbose    0 ;# Level of chattering written during a run.
	variable src       {} ;# List of files to process.

	variable actualplatform {} ;# Target platform, with x-compile information resolved.

	variable shlname   "" ;# Name of the shlib to generate (-lib, -pkg).
	variable outname   "" ;# Name of the shlib dir to use (-lib, -pkg).
	variable libdir   lib ;# Place for the package (-pkg).
	variable keep       0 ;# Boolean flag. Default: Do not keep generated .c files.
	variable debug     {} ;# List of debug modes to activate.
	variable cache     {} ;# User specified path to the directory for the result cache.

	# Build mode. Default, as below is 'build and link input
	# files, do not load, fill cache'. If the flag is set a
	# package is made instead.

	variable pkg 0

	# - -- --- ----- -------- ------------- ---------------------
	# Data accumulated while processing the input files.

	variable failed      0  ;# Number of build failures encountered.
	variable clibraries {}  ;# External libraries used. To link the final shlib against.
	variable ldflags    {}  ;# Linker flags.
	variable objects    {}  ;# The object files to link.
	variable edecls     {}  ;# Initialization function decls for the pieces (C code block).
	variable initnames  {}  ;# Initialization function calls for the pieces (C code block).
	variable tsources   {}  ;# Tcl companion sources.
	variable mintcl     8.4 ;# Minimum version of Tcl required to run the package.
	variable preload    {}  ;# List of libraries declared for preload.
	variable license    {}  ;# Accumulated licenses, if any.
	variable pkgs       {}  ;# List of package names for the pieces.
	variable inits      {}  ;# Init function names for the pieces, list.
    }
}

# # ## ### ##### ######## ############# #####################
return
