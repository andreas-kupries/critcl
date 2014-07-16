# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the cc configuration data used by the
# standard backend (using an external C compiler).

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4        ;# Minimal supported Tcl runtime.
package require critcl::cache  ;# Result cache access (See 'use').
package require critcl::common ;# Common utilities
package require lassign84      ;# Forward compatible lassign
package require dict84         ;# Forward compatible dict

# # ## ### ##### ######## ############# #####################

package provide  critcl::ccconfig 1
namespace eval ::critcl::ccconfig {
    namespace export showall show get use choose read \
	known target targetpplatform buildplatform \
	sharedlibext crosscheck do do-log
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::ccconfig::do {failvar rv cmdline} {
    upvar 1 $failvar failed $rv result
    variable cip

    set failed [catch {
	interp eval $cip [linsert $cmdline 0 exec]
    } result]

    return [expr {!$failed}]
}

proc ::critcl::ccconfig::do-log {failvar rv log cmdline} {
    upvar 1 $failvar failed $rv result
    variable cip

    # Extend the command, redirect all of its output (stdout and
    # stderr) into the current log.
    lappend cmdline >&@ $log

    interp transfer {} $log $cip

    set failed [catch {
	interp eval $cip [linsert $cmdline 0 exec]
    } result]

    interp transfer $cip $log {}

    return [expr {!$failed}]
}

# See (XX) at the end of the file (package state variable setup)
# for explanations of the exact differences between these.

proc ::critcl::ccconfig::known {} {
    variable knowntargets
    return  $knowntargets
}

proc ::critcl::ccconfig::target {} {
    variable target
    return  $target
}

proc ::critcl::ccconfig::targetplatform {} {
    variable targetplatform
    return  $targetplatform
}

proc ::critcl::ccconfig::buildplatform {} {
    variable buildplatform
    return  $buildplatform
}

proc ::critcl::ccconfig::actual {} {
    # Check if the chosen target is a cross-compile target.  If yes,
    # we return the actual platform identifier of the target. This is
    # used to select the proper platform directory names in the critcl
    # cache, generated packages, when searching for preload libraries,
    # etc. Whereas the chosen target provides the proper compile
    # configuration which will invoke the proper cross-compiler, etc.

    variable xtargets
    variable targetplatform

    if {[info exists xtargets($targetplatform)]} {
	return $xtargets($targetplatform)
    } else {
	return $targetplatform
    }
}

proc ::critcl::ccconfig::sharedlibext {} {
    return [get sharedlibext]
}

proc ::critcl::ccconfig::crosscheck {} {
    variable xtargets

    # Run the 'version' command
    set     cmd [get version]
    lappend cmd 2> [Null]
    if {[do _ config $cmd]} {
	# Was successful, parse the returned string, aka stdout.
	set host   ""
	set target ""
	foreach line $config {
	    foreach arg [split $line] {
		if {[string match "--*" $arg]} {
		    lassign [split [string trim $arg -] =] cfg val
		    set $cfg $val
		}
	    }
	}
	if {($host ne $target) &&
	    [info exists xtargets($target)]
	} {
	    # Change target
	    use $target
	    print stderr "Cross compiling using $target"
	}
	# XXX host != target, but not known in the configuration ?
	# XXX Currently ignored.
	# XXX Would throwing an error be better ?
    }
    return
}

proc ::critcl::ccconfig::showall {{ofd {}}} {
    variable datafile
    if {$ofd ne ""} {
	set ifd [open $datafile r]
	fcopy $ifd $ofd
	close $ifd
    } else {
	return [common::cat $datafile]
    }
}

proc ::critcl::ccconfig::show {{fd {}}} {
    variable datafile
    variable buildplatform
    variable targetplatform
    variable tools
    variable current

    if {$targetplatform eq ""} {
	set plat "default"
    } else {
	set plat $targetplatform
    }

    set out [list]
    if {$plat eq $buildplatform} {
	lappend out "Config: $plat"
    } else {
	lappend out "Config: $plat (built on $buildplatform)"
    }
    lappend out "Origin: $datafile"

    # Get the full target configuration.
    set definitions [dict get tools $targetplatform]

    # Show the config variables first.
    # "cache" is a pseudo-variable.

    set max [common::maxlen [dict keys $current]]
    lappend out "    [format %-${max}s cache] [cache::get]"

    foreach var [lsort -dict [dict keys $current]] {
	if {![dict exists $definitions $var]} continue

	set val [Resolve [dict get $definitions $var]]
	set line "    [format %-${max}s $var]"

	# Break value across lines (assuming a width of 70).
	# XXX Use textutil::adjust+indent instead ?
	foreach word [split [string trim $val]] {
	    if {[set word [string trim $word]] eq ""} continue
	    if {[string length "$line $word"] > 70} {
		lappend out "$line \\"
		set line "    [format %-${max}s { }] $word"
	    } else {
		set line "$line $word"
	    }
	}
	lappend out $line
    }

    # Now show the part of the config which are not known variables,
    # and are thus plain Tcl. If any.

    set tclvars [list]
    foreach var [dict keys $definitions] {
	if {($var eq "when")} continue
	if {[dict exists $current $var]} continue
	lappend tclvars $var
    }
    if {[llength $tclvars]} {
	set max [common::maxlen $tclvars]

	lappend out "Tcl variables:"
	foreach var $tclvars {
	    set val [dict get $definitions $var]
	    if {[llength $val] == 1} {
		# for when someone inevitably puts quotes around
		# values - e.g. "Windows NT"
		set val [lindex $val 0]
	    }
	    lappend out "    [format %-${max}s $var] $val"
	}
    }

    # All assembled, now we can write or return the result.

    set out [join $out \n]
    if {$fd ne ""} {
	puts $fd $out
    } else {
	return $out
    }
}

proc ::critcl::ccconfig::get {var} {
    variable current
    return [Resolve [dict get $current $var]]
}

proc ::critcl::ccconfig::choose {targetid {err 0}} {
    variable knowntargets

    # first try to match exactly
    set match [lsearch -exact -all -inline $knowntargets $targetid]

    # on failure, try to match as glob pattern
    if {![llength $match]} {
        set match [lsearch -glob -all -inline $knowntargets $targetid]
    }

    # on failure, error out if requested
    if {![llength $match] && $err} {
	set choices [linsert [join [lsort -dict $knowntargets] {, }] end-1 or]
	return -code error \
	    -errorcode {CRITCL CCCONFIG INVALID TARGET-ID} \
	    "Invalid target \"$target\", must be one of $choices"
    }
    return $match
}

proc ::critcl::ccconfig::use {targetconfig} {
    variable target         $targetconfig
    variable targetplatform $targetconfig
    variable buildplatform
    variable tools
    variable version

    # Strip the compiler information from the configuration to get the
    # platform identifier embedded into it. This is a semi-recurrence
    # of the original hardwired block handling win32/gcc/cl. We can
    # partly emulate this with 'platform' directives in the Config
    # file, however this breaks down when trying to handle the default
    # settings. I.e. something like FOO-gcc which has no configuration
    # block in the file uses the defaults, and thus has no proper
    # place for a custom platform directive. So we have to do it here,
    # in code. For symmetry the other compilers (-cc, -cl) are handled
    # as well.

    foreach p {gcc cc_r xlc xlc_r cc cl} {
	if {[regsub -- "-$p\$" $targetplatform {} vtargetplatform]} break
    }

    # (**) Defaults.
    dict set current platform     ""
    dict set current sharedlibext ""

    # Copy the chosen configuration into the current one.
    # Get the full target configuration.

    set definitions [dict get tools $targetconfig]
    foreach var [dict keys $current] {
	if {![dict exists $definitions $var]} continue
	set val [dict get $definitions $var]
	dict set current $var $val
	# Note: Resolve happens on actual use.

	if {$var ne "platform"} continue
	lassign [Resolve $val] targetplatform version
    }
    if {[info exists ::env(CFLAGS)]} {
	dict append current compile " $::env(CFLAGS)"
    }
    if {[info exists ::env(LDFLAGS)]} {
	dict append current link         " $::env(LDFLAGS)"
	dict append current link_preload " $::env(LDFLAGS)"
    }
    if {[string match $targetplatform $buildplatform]} {
	# expand platform to match host if it contains wildcards
	set targetplatform $buildplatform
    }

    # Override defaults (see **) with more sensible information.
    if {[dict get $current platform] eq ""} {
	# default config platform (mainly for the "show" command)
	dict set current platform $targetplatform
    }
    if {[dict get $current sharedlibext] eq ""} {
	dict set current sharedlibext [info sharedlibextension]
    }

    cache::def [file join ~ .critcl $targetplatform]

    # Now take the part of the config which are not known variables,
    # and are thus plain Tcl. If any. Set them XXX ?WHERE?

    foreach var [dict keys $definitions] {
	if {($var eq "when")} continue
	if {[dict exists $current $var]} continue

	set val [dict get $definitions $var]
	if {[llength $val] == 1} {
	    # for when someone inevitably puts quotes around
	    # values - e.g. "Windows NT"
	    set val [lindex $val 0]
	}
	set $var $val
    }
    return
}

proc ::critcl::ccconfig::read {config} {
    variable cip
    variable buildplatform
    variable current

    set cfg [open $config]
    set knowntargets [list]
    set cont ""
    set whenplat ""

    interp eval $cip set platform $buildplatform

    set i 0
    while {[gets $cfg line] >= 0} {
	incr i
	if {[set line [string trim $line]] ne ""} {
	    # config lines can be continued using trailing backslash
	    if {[string index $line end] eq "\\"} {
		append cont " [string range $line 0 end-1]"
		continue
	    }
	    if {$cont ne ""} {
		append cont $line
		set line [string trim $cont]
		set cont ""
	    }

	    # At this point we have a complete line/command in 'line'.
	    # We expect the following forms of input:
	    #
	    # (1.) if {...} {.............} - Tcl command, run in the
	    #                                 backend interpreter.
	    #                                 Note that this can EXIT
	    #                                 the application using
	    #                                 the critcl package.
	    # (2.)  set VAR VALUE.......... - Ditto.
	    # (3.)  # ..................... - Comment. Skipped
	    # (4.) PLATFORM VAR VALUE...... - Platform-specific
	    #                                 configuration variable
	    #                                 and value.

	    # (4a) PLATFORM when .........  - Makes the PLATFORM
	    #                                 conditional on the
	    #                                 expression after the
	    #                                 'when' keyword. This
	    #                                 uses variables set by
	    #                                 (1) and/or (2). The
	    #                                 expression is run in the
	    #                                 backend interpreter. If
	    #                                 and only if PLATFORM is
	    #                                 a prefix of the current
	    #                                 build platform, or the
	    #                                 reverse, then the code
	    #                                 with an TRUE when is
	    #                                 chosen as the
	    #                                 configuration.

	    # (4b) PLATFORM target ?actual? - Marks the platform as a
	    #                                 cross-compile target,
	    #                                 and actual is the
	    #                                 platform identifier of
	    #                                 the result. If not
	    #                                 specified it defaults to
	    #                                 PLATFORM.
            # (4c) PLATFORM copy PARENT...  - Copies the currently defined
            #                                 configuration variables and
            #                                 values to the settings for 
            #                                 this platform.
	    # (5.) VAR VALUE............... - Default configuration
	    #                                 variable, and value.

	    set plat [lindex [split $line] 0]

	    # (1), or (2)
	    if {$plat eq "set" || $plat eq "if"} {
		while {![info complete $line] && ![eof $cfg]} {
		    if {[gets $cfg more] == -1} {
			set msg "incomplete command in Critcl Config file "
			append msg "starting at line $i"
			error $msg
		    }
		    append line  "\n$more"

		}
		interp eval $cip $line
		continue
	    }

	    # (3)
	    if {$plat eq "#"} continue

	    # (4), or (5).
	    if {[lsearch -exact [dict keys $current] $plat] != -1} {
		# XXX FIXME USE dict exists $current $plat

		# (5) default config option
		set cmd ""
		if {![regexp {(\S+)\s+(.*)} $line -> type cmd]} {
		    # cmd is empty
		    set type $plat
		    set cmd ""
		}
		set plat ""
	    } else {
		# (4) platform config option
		if {![regexp {(\S+)\s+(\S+)\s+(.*)} $line -> p type cmd]} {
		    # cmd is empty
		    set type [lindex $line 1]
		    set cmd ""
		}

		# (4a) if and only if either build platform or config
		#      code are a prefix of each other can the 'when'
		#      condition be evaluated and override the
		#      standard selection for the configuration.

		if {$type eq "when" &&
		    ( [string match ${buildplatform}* $plat] ||
		      [string match ${plat}* $buildplatform] )} {
		    set res ""
		    catch {
			set res [interp eval $cip expr $cmd]
		    }
		    switch $res {
			"" -
			0 { set whenfalse($plat) 1 }
			1 { set whenplat $plat }
		    }
		}
		lappend knowntargets $plat
	    }

            switch -exact -- $type {
                target {
                    # (4b) cross compile target.
                    # cmd = actual target platform identifier.
                    if {$cmd eq ""} {
                        set cmd $plat
                    }
                    set xtargets($plat) $cmd
                }
                copy {
                    # (4c) copy an existing config
                    # XXX - should we error out if no definitions exist
                    # for parent platform config
                    # $cmd contains the parent platform
		    if {[dict exists $tools $cmd]} {
			dict for {key val} [dict get $tools $cmd] {
			    dict set tools $plat $key $val
			}
		    }
                }
                default {
                    dict set tools $plat $type $cmd
                }
	    }
	}
    }
    set knowntargets [lsort -unique $knowntargets]
    close $cfg

    # Config file processing has completed.
    # Now select the platform to configure the
    # compiler backend with.
    #
    # The config file may have selected a configuration based on the
    # TRUE when conditions. Which were matched to buildplatform,
    # making the chosen config a variant of it. If that did not happen
    # a platform is chosen from the set of defined targets.
    if {$whenplat ne ""} {
	set match [list $whenplat]
    } else {
	set match [choose $buildplatform]
    }

    # Initial configuration of the backend.

    use ""    ;# defaults
    if {[llength $match]} {
	use [lindex $match 0]
    } else {
	use $buildplatform
    }
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::ccconfig {
    namespace eval common { namespace import ::critcl::common::* }
    namespace eval cache  { namespace import ::critcl::cache::*  }

    # String. Min version number on platform.
    # XXX It is unclear who/where this is actually used.
    # XXX OS X maybe ?
    variable version ""

    # (XX) To understand the set of variables below and their
    # differences some terminology is required.
    #
    # First we have to distinguish between "target identifiers" and
    # "platform identifiers". The first is the name for a particular
    # set of configuration settings specifying commands and command
    # line arguments to use. The second is the name of a machine
    # configuration, identifying both operating system, and cpu
    # architecture.
    #
    # The problem critcl has is that in 99% of the cases found in a
    # critcl config file the "target identifier" is also a valid
    # "platform identifier". Example: "linux-ix86". That does not make
    # them semantically interchangable however.
    #
    # Especially when we add cross-compilation to the mix, where we
    # have to further distinguish between the platform critcl itself
    # is running on (build), and the platform for which critcl is
    # generating code (target), and the last one sounds similar to
    # "target identifier".

    variable target          ;# Target identifier. The chosen configuration.
    variable targetplatform  ;# Platform identifier. We generate binaries for there.
    variable buildplatform   ;# Platform identifier. We run here.

    variable knowntargets {} ;# List of all target identifiers found
                              # in the configuration file last processed
                              # by "read".
	
    variable  xtargets       ;# Cross-compile targets. This array maps from
    array set xtargets {}    ;# the target identifier to the actual platform
    # identifier of the target platform in question. If a target identifier
    # has no entry here, it is assumed to be the platform identifier itself.
    # See "critcl::actualtarget".

    # Interpreter used to resolve configuration values. As they may contain
    # references to other variables.
    variable cip

    # Database mapping from target identifier and variable name to its
    # value. In-memory form of the configuration file.  When a target
    # is chosen the values which config are copied into "current", see
    # below.
    #
    # dict: (target -> (varname -> value))
    variable tools {}

    # Dictionary holding the currently chosen compiler configuration
    # (commands and options for the various tasks, i.e. compilation,
    # linking, etc.). They keys of this dictionary are implicitly
    # naming all the known and legal configuration variables. The full
    # details regarding their meaning and use can be found in the
    # standard configuration file "Config" which is part of the
    # package. Keep this information in sync with the contents of
    # 'Config'.

    variable current {
	compile        {}
	debug_memory   {}
	debug_symbols  {}
	include        {}
	libinclude     {}
	ldoutput       {}
	embed_manifest {}
	link           {}
	link_debug     {}
	link_preload   {}
	link_release   {}
	noassert       {}
	object	       {}
	optimize       {}
	output         {}
	platform       {}
	preproc_define {}
	preproc_enum   {}
	sharedlibext   {}
	strip	       {}
	tclstubs       {}
	threadflags    {}
	tkstubs        {}
	version        {}
    }

    # compile         Command to compile a C source file to an object file
    # debug_memory    Compiler flags to enable memory debugging
    # debug_symbols   Compiler flags to add symbols to resulting library
    # include         Compiler flag to add an include directory
    # libinclude      Linker flag to add a library directory
    # ldoutput       - ? See 'Config'
    # link            Command to link one or more object files and create a shared library
    # embed_manifest  Command to embed a manifest into a DLL. (Win-specific)
    # link_debug     - ? See 'Config'
    # link_preload   Linker flags to use when dependent libraries are pre-loaded.
    # link_release   - ? See 'Config'
    # noassert        Compiler flag to turn off assertions in Tcl code
    # object          File extension for object files
    # optimize        Compiler flag to specify optimization level
    # output          Compiler flag to set output file, with argument $object => Use via [subst].
    # platform        Platform identification string (defaults to platform::generic)
    # preproc_define  Command to preprocess C source file (for critcl::cdefines)
    # preproc_enum    ditto
    # sharedlibext    The platform's file extension used for shared library files.
    # strip           Compiler flag to tell the linker to strip symbols
    # target          Presence of this key indicates that this is a cross-compile target
    # tclstubs        Compiler flag to set USE_TCL_STUBS
    # threadflags     Compiler flags to enable threaded build
    # tkstubs         Compiler flag to set USE_TK_STUBS
    # version         Command to print the compiler version number
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::ccconfig::Resolve {value} {
    variable cip
    catch {
	set value [interp eval $cip [list subst $value]]
    }
    return $value
}

proc ::critcl::ccconfig::BuildPlatform {} {
    set platform [::platform::generic]

    # Behave like a autoconf generated configure
    # - $CC (user's choice first)
    # - gcc, if available.
    # - cc/cl otherwise (without further check for availability)

    if {[info exists ::env(CC)]} {
	# The compiler may be a gcc, despite being named .../cc.

	set cc $::env(CC)
	if {[IsGCC $cc]} {
	    set cc gcc
	}
    } elseif {[llength [auto_execok gcc]]} {
	set cc gcc
    } else {
	if {[string match "win32-*" $platform]} {
	    set cc cl
	} else {
	    set cc cc
	}
    }

    # The cc may be a full path, through the CC environment variable,
    # which is bad for use in the platform code. Use only the last
    # element of said path, without extensions (.exe). And it may be
    # followed by options too, so look for and strip these off as
    # well. This last part assumes that the path of the compiler
    # itself doesn't contain spaces.

    regsub {( .*)$} [file tail $cc] {} cc
    append platform -[file rootname $cc]

    return $platform
}

proc ::critcl::ccconfig::IsGCC {path} {
    if {[catch {
	set lines [exec $path -v |& grep gcc]
    }] || ($lines eq {})} { return 0 }
    return 1
}

proc ::critcl::ccconfig::Null {} {
    global tcl_platform
    if {$tcl_platform(platform) eq "windows"} {
	return NUL:
    } else {
	return /dev/null
    }
}

# # ## ### ##### ######## ############# #####################
## Initialization

proc ::critcl::ccconfig::Initialize {} {
    variable cip [interp create]
    variable datafile
    variable buildplatform [BuildPlatform]

    set selfdir  [file dirname [file normalize [info script]]]
    set datafile [file join $selfdir Config]
    # Path to the package directory.
    # Load the default configuration file.
    # This also chooses and sets the target platform.
    read $datafile

    # XXX FIXME - remove IsGCC, BuildPlatform as well
    rename ::critcl::ccconfig::Initialize {}
    return
}
::critcl::ccconfig::Initialize

# # ## ### ##### ######## ############# #####################
## Ready
return
