# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the static scanner used to extract dependencies, meta data,
# related files, etc from .critcl files.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4          ;# Minimal supported Tcl runtime.

package provide  critcl::scan 1
namespace eval ::critcl::scan {
    namespace export
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::scan {file} {
    set lines [split [scan::Cat $file] \n]

    # XXX move into local state
    set scan::rkey    require
    set scan::base    [file dirname [file normalize $file]]
    set scan::capture {
	org         {}
	version     {}
	files       {}
	imported    {}
	config      {}
	meta-user   {}
	meta-system {}
	tsources    {}
    }

    scan::Core $lines {
	critcl::api			sub
	critcl::api/extheader		ok
	critcl::api/function		ok
	critcl::api/header		warn
	critcl::api/import		ok
	critcl::source                  warn
	critcl::cheaders		warn
	critcl::csources		warn
	critcl::license			warn
	critcl::meta			warn
	critcl::owns			warn
	critcl::tcl			ok
	critcl::tk			ok
	critcl::tsources		warn
	critcl::userconfig		sub
	critcl::userconfig/define	ok
	critcl::userconfig/query	ok
	critcl::userconfig/set		ok
	package				warn
    }

    set version [dict get $scan::capture version]
    print "\tVersion:      $version"

    # TODO : Report requirements.

    set n [llength [dict get $scan::capture files]]
    print -nonewline "\tInput:        $file"
    if {$n} {
	print -nonewline " + $n Companion"
	if {$n > 1} { print -nonewline s }
    }
    print ""

    # Merge the system and user meta data, with system overriding the
    # user. See 'GetMeta' for same operation when actually builing the
    # package. Plus scan any Tcl companions for more requirements.

    set     md {}
    lappend md [dict get $scan::capture meta-user]
    lappend md [dict get $scan::capture meta-system]

    foreach ts [dict get $scan::capture tsources] {
	lappend md [dict get [scan-dependencies $file \
				  [file join [file dirname $file] $ts] \
				  capture] meta-system]
    }

    dict unset scan::capture meta-user
    dict unset scan::capture meta-system
    dict unset scan::capture tsources

    dict set scan::capture meta \
	[eval [linsert $md 0 dict merge]]
    # meta = dict merge {*}$md

    if {[dict exists $scan::capture meta require]} {
	foreach r [dict get $scan::capture meta require] {
	    print "\tRequired:     $r"
	}
    }

    return $scan::capture
}

proc ::critcl::scan-dependencies {dfile file {mode plain}} {
    set lines [split [scan::Cat $file] \n]

    # XXX local state
    catch {
	set saved $scan::capture
    }

    set scan::rkey    require
    set scan::base    [file dirname [file normalize $file]]
    set scan::capture {
	name        {}
	version     {}
	meta-system {}
    }

    scan::Core $lines {
	critcl::buildrequirement	warn
	package				warn
    }

    if {$mode eq "capture"} {
	set result $scan::capture
	set scan::capture $saved
	return $result
    }

    dict with scan::capture {
	if {$mode eq "provide"} {
	    # XXX back reference into critcl core
	    ::critcl::msg -nonewline " (provide $name $version)"
	    ::critcl::ImetaSet $dfile name     $name
	    ::critcl::ImetaSet $dfile version  $version
	}

	dict for {k vlist} [dict get $scan::capture meta-system] {
	    if {$k eq "name"}    continue
	    if {$k eq "version"} continue

	    # XXX back reference into critcl core
	    ::critcl::ImetaAdd $dfile $k $vlist

	    if {$k ne "require"} continue
	    ::critcl::msg -nonewline " ($k [join $vlist {}])"
	}

	# The above information also goes into the teapot meta data of
	# the file in question. This however is defered until the meta
	# data is actually pulled for delivery to the tool using the
	# package. See 'GetMeta' for where the merging happens.
    }

    return
}

proc critcl::scan::Core {lines theconfig} {
    # config = dictionary
    # - <cmdname> => mode (ok, warn, sub)
    # Unlisted commands are ignored.

    variable scan::config $theconfig

    set collect 0
    set buf {}
    set lno -1
    foreach line $lines {
	#puts |$line|

	incr lno
	if {$collect} {
	    if {![info complete $buf]} {
		append buf $line \n
		continue
	    }
	    set collect 0

	    #puts %%$buf%%

	    # Prevent heavily dynamic code from stopping the scan.
	    # WARN the user.
	    regexp {^(\S+)} $buf -> cmd
	    if {[dict exists $config $cmd]} {
		set mode [dict get $config $cmd]

		if {[catch {
		    # Run in the scan namespace, with its special
		    # command implementations.
		    namespace eval ::critcl::scan $buf
		} msg]} {
		    if {$mode eq "sub"} {
			regexp {^(\S+)\s+(\S+)} $buf -> _ method
			append cmd /$method
			set mode [dict get $config $cmd]
		    }
		    if {$mode eq "warn"} {
			# XXX back reference into critcl core
			::critcl::msg "Line $lno, $cmd: Failed execution of dynamic command may"
			::critcl::msg "Line $lno, $cmd: cause incorrect TEA results. Please check."
			::critcl::msg "Line $lno, $cmd: $msg"
		    }
		}
	    }

	    set buf ""
	    # fall through, to handle the line which just got NOT
	    # added to the buf.
	}

	set line [string trimleft $line " \t:"]
	if {[string trim $line] eq {}} continue

	regexp {^(\S+)} $line -> cmd
	if {[dict exists $config $cmd]} {
	    append buf $line \n
	    set collect 1
	}
    }
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::scan {
}

# # ## ### ##### ######## ############# #####################
## Internal support commands
# Mainly scanner-specific replacements of core critcl commands, and
# some Tcl builtin commands.

# Handle the extracted commands
namespace eval ::critcl::scan::critcl {}

proc ::critcl::scan::critcl::buildrequirement {script} {
    # Recursive scan of the script, same configuration, except
    # switched to record 'package require's under the build::reqire
    # key.

    variable ::critcl::scan::config
    variable ::critcl::scan::rkey

    set saved $rkey
    set rkey build::require

    ::critcl::scan::Core [split $script \n] $config

    set rkey $saved
    return
}

# Meta data.
# Capture specific dependencies
proc ::critcl::scan::critcl::tcl {version} {
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	dict lappend m require [list Tcl $version]
    }
    return
}

proc ::critcl::scan::critcl::tk {} {
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	dict lappend m require Tk
    }
    return
}

proc ::critcl::scan::critcl::description {text} {
    variable ::critcl::scan::capture
    dict set capture meta-system description \
	[::critcl::Text2Words $text]
    # XXX back reference into critcl core
    return
}

proc ::critcl::scan::critcl::summary {text} {
    variable ::critcl::scan::capture
    dict set capture meta-system summary \
	[::critcl::Text2Words $text]
    # XXX back reference into critcl core
    return
}

proc ::critcl::scan::critcl::subject {args} {
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	foreach word $args {
	    dict lappend m subject $word
	}
    }
    return
}

proc ::critcl::scan::critcl::meta {key args} {
    variable ::critcl::scan::capture
    dict update capture meta-user m {
	foreach word $args {
	    dict lappend m $key $word
	}
    }
    return
}

# Capture files
proc ::critcl::scan::critcl::source   {path} {
    # Recursively scan the imported file.
    # Keep the current context.
    variable ::critcl::scan::config

    # XXX in-scanner cross-level references.
    foreach f [Files $path] {
	set lines [split [::critcl::scan::Cat $f] \n]
	::critcl::scan::Core $lines $config
    }
    return
}
proc ::critcl::scan::critcl::owns     {args} { eval [linsert $args 0 Files] }
proc ::critcl::scan::critcl::cheaders {args} { eval [linsert $args 0 Files] }
proc ::critcl::scan::critcl::csources {args} { eval [linsert $args 0 Files] }
proc ::critcl::scan::critcl::tsources {args} {
    variable ::critcl::scan::capture
    foreach ts [eval [linsert $args 0 Files]] {
	dict lappend capture tsources $ts
    }
    return
}

# Capture license (org name)
proc ::critcl::scan::critcl::license {who args} {
    variable ::critcl::scan::capture
    dict set capture org $who

    print "\tOrganization: $who"

    # Meta data.
    set elicense [::critcl::LicenseText $args]
    # XXX back reference into critcl core

    dict set capture meta-system license \
	[::critcl::Text2Words $elicense]
    dict set capture meta-system author \
	[::critcl::Text2Authors $who]
    # XXX back reference into critcl core
    return
}

# Capture version of the provided package.
proc ::critcl::scan::package {cmd args} {
    if {$cmd eq "provide"} {
	# Syntax: package provide <name> <version>

	variable capture
	lassign $args name version
	dict set capture name    $name
	dict set capture version $version

	# Save as meta data as well.

	dict set capture meta-system name     $name
	dict set capture meta-system version  $version
	dict set capture meta-system platform source
	dict set capture meta-system generated::by \
	    [list \
		 [list critcl [::package present critcl]] \
		 $::tcl_platform(user)]
	dict set capture meta-system generated::date \
	    [list [clock format [clock seconds] -format {%Y-%m-%d}]]
	return
    } elseif {$cmd eq "require"} {
	# Syntax: package require <name> ?-exact? <version>
	#       : package require <name> <version-range>...

	# Save dependencies as meta data.

	# Ignore the critcl core
	if {[lindex $args 0] eq "critcl"} return

	variable capture
	variable rkey
	dict update capture meta-system m {
	    dict lappend m $rkey [::critcl::TeapotRequire $args]
	    # XXX back reference into critcl core
	}
	return
    }

    # ignore anything else.
    return
}

# Capture the APIs imported by the package
proc ::critcl::scan::critcl::api {cmd args} {
    variable ::critcl::scan::capture
    switch -exact -- $cmd {
	header {
	    eval [linsert $args 0 Files]
	}
	import {
	    # Syntax: critcl::api import <name> <version>
	    lassign $args name _
	    dict lappend capture imported $name
	    print "\tImported:     $name"
	}
	default {}
    }
    return
}

# Capture the user config options declared by the package
proc ::critcl::scan::critcl::userconfig {cmd args} {
    variable ::critcl::scan::capture
    switch -exact -- $cmd {
	define {
	    # Syntax: critcl::userconfig define <name> <description> <type> ?<default>?
	    lassign $args oname odesc otype odefault
	    set odesc [string trim $odesc]
	    if {[llength $args] < 4} {
		set odefault [::critcl::UcDefault $otype]
		# XXX back reference into critcl core
	    }
	    dict lappend capture config [list $oname $odesc $otype $odefault]
	    print "\tUser Config:  $oname ([join $otype { }] -> $odefault) $odesc"
	}
	set - query -
	default {}
    }
    return
}

# # ## ### ##### ######## ############# #####################
## Full internal helper commands.

proc ::critcl::scan::critcl::Files {args} {
    variable ::critcl::scan::capture
    set res {}
    foreach v $args {
	if {[string match "-*" $v]} continue
	foreach f [Expand $v] {
	    dict lappend capture files $f
	    lappend res $f
	}
    }
    return $res
}

proc ::critcl::scan::critcl::Expand {pattern} {
    variable ::critcl::scan::base

    # Note: We cannot use -directory here. The PATTERN may already be
    # an absolute path, in which case the join will return the
    # unmodified PATTERN to glob on, whereas with -directory the final
    # pattern will be BASE/PATTERN which won't find anything, even if
    # PATTERN actually exists.

    set prefix [file split $base]

    set files {}
    foreach vfile [glob [file join $base $pattern]] {
	set xfile [file normalize $vfile]
	if {![file exists $xfile]} {
	    error "$vfile: not found"
	}

	# Constrain to be inside of the base directory.
	# Snarfed from fileutil::stripPath

	set npath [file split $xfile]

	if {![string match -nocase "${prefix} *" $npath]} {
	    error "$vfile: Not inside of $base"
	}

	set xfile [eval [linsert [lrange $npath [llength $prefix] end] 0 file join ]]
	lappend files $xfile
    }
    return $files
}

proc ::critcl::scan::Cat {path} {
    # Easier to write our own copy than requiring fileutil and then
    # using fileutil::cat.

    set fd [open $path r]
    set data [read $fd]
    close $fd
    return $data
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
