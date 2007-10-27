#
#   Critcl - build C extensions on-the-fly
#
#   Copyright (c) 2001-2006 Jean-Claude Wippler
#   Copyright (c) 2002-2006 Steve Landers
#
#   See http://www.purl.org/tcl/wiki/critcl
#
#   This is the Critcl runtime that loads the appropriate
#   shared library when a package is requested
#

namespace eval ::critcl {

    proc loadlib {dir package version args} {
        global tcl_platform
        set path [file join $dir [::critcl::platform]]
        set ext [info sharedlibextension]
        set lib [file join $path $package$ext]
        set provide [list]
        if {[llength $args]} {
            lappend provide [list load [file join $path preload$ext]]
            foreach p $args {
                lappend provide [list @preload [file join $path $p$ext]]
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

    # a version of critcl::platform that applies the platform mapping
    proc platform {} {
        set platform [::platform::generic]
        set version $::tcl_platform(osVersion)
        if {[string match "*-macosx" $platform]} {
            # "normalize" the osVersion to match OSX release numbers
            set v [split $version .]
            set v1 [lindex $v 0]
            set v2 [lindex $v 1]
            incr v1 -4
            set version 10.$v1.$v2
        }
        foreach {config map} $::critcl::mapping {
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
