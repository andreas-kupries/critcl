#
#   Critcl - build C extensions on-the-fly
#
#   Copyright (c) 2001-2007 Jean-Claude Wippler
#   Copyright (c) 2002-2007 Steve Landers
#
#   See http://wiki.tcl.tk/critcl
#
#   This is the Critcl runtime that loads the appropriate
#   shared library when a package is requested
#

namespace eval ::critcl2 {

    proc loadlib {dir package version mapping args} {
        global tcl_platform
        set path [file join $dir [::critcl::platform $mapping]]
        set ext [info sharedlibextension]
        set lib [file join $path $package$ext]
        set provide [list]
	if {[llength $args]} {
            set preload [file join $path preload$ext]
	    foreach p $args {
		set prelib [file join $path $p$ext]
		if {[file readable $preload] && [file readable $prelib]} {
		    lappend provide [list load $preload]
                    lappend provide [list ::critcl2::preload $prelib]
                }
            }
        }
        lappend provide [list load $lib $package]
        foreach t [glob -nocomplain [file join $dir Tcl *.tcl]] {
            lappend provide [list source $t]
        }
        lappend provide "package provide $package $version"
        package ifneeded $package $version [join $provide "; "]
        package ifneeded critcl 0.0 \
         "package provide critcl 0.0; [list source [file join $dir critcl.tcl]]"
    }

    # ::critcl2::precopy is only used on Windows when preloading out of a
    # VFS that doesn't support direct loading (usually, a Starkit)
    #   - we preserve the dll name so that dependencies are satisfied
    #	- critcl2::preload is defined in critcl/lib/critcl/critcl_c/preload.c

    proc precopy {dll} {
	global env
	if {[info exists env(TEMP)]} {
	    set dir $env(TEMP)
	} elseif {[info exists env(TMP)]} {
	    set dir $env(TMP)
	} elseif {[info exists ~]} {
	    set dir ~
	} else {
	    set dir .
	}
	set dir [file join $dir TCL[pid]]
	set i 0
	while {[file exists $dir]} {
	    append dir [incr i]
	}
	set new [file join $dir [file tail $dll]]
	file mkdir $dir
	file copy $dll $new
	return $new
    }
}

namespace eval ::critcl {
    # a version of critcl::platform that applies the platform mapping
    proc platform {{mapping ""}} {
        set platform [::platform::generic]
        set version $::tcl_platform(osVersion)
        if {[string match "macosx-*" $platform]} {
            # "normalize" the osVersion to match OSX release numbers
            set v [split $version .]
            set v1 [lindex $v 0]
            set v2 [lindex $v 1]
            incr v1 -4
            set version 10.$v1.$v2
        }
        foreach {config map} $mapping {
            if {[string match $config $platform]} {
                set minver [lindex $map 1]
                if {[package vcompare $version $minver] != -1} {
                    set platform [lindex $map 0]
                    break
                }
            }
        }
        return $platform
    }

}
