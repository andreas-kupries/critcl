## -*- tcl -*-
# -------------------------------------------------------------------------

proc get {args} {
    set t [string trim [critcl::collect $args]]
    #regsub -all -- {#line \d+ } $t {#line XX } t
    return $t
}

proc setup {} {
    critcl::fastuuid                     ;# no md5, counter, reset
    set critcl::v::this FAKE             ;# fake a file-name
    critcl::cache [localPath test/CACHE] ;# choose cache
    return
}

proc the-file {} {
    return $critcl::v::this
}

proc cleanup {} {
    unset critcl::v::this
    unset critcl::v::code
    unset critcl::v::delproc
    unset critcl::v::clientdata
    file delete -force -- [localPath test/CACHE]
    return
}

proc inspect {pattern} {
    viewFile [lindex [glob -directory [critcl::cache] $pattern] 0]
}

proc overrides {} {
    critcl::fastuuid         ;# no md5, use counter
    critcl::config lines   0 ;# no #line, mostly
    critcl::config keepsrc 1 ;# keep sources in cache

    # Disable actual compilation
    proc ::critcl::Compile {tclfile origin cfile obj} {
    }

    proc ::critcl::Link {file} {
    }

    proc ::critcl::ExecWithLogging {cmdline okmsg errmsg} {
    }

    proc ::critcl::Exec {cmdline} {
    }

    proc ::critcl::Load {file} {
    }
}


tcltest::customMatch glob-check G
proc G {p s} {
    set map [list \n \\n \t \\t { } \\s]
    set buf {}
    foreach c [split $p {}] {
        append buf $c
        if {![string match ${buf}* $s]} {
	    puts FAIL|[string map $map ${buf}*]|
        }
    }
    return 1
}

tcltest::testConstraint tcl86plus [package vsatisfies [package present Tcl] 8.6]
tcltest::testConstraint tcl85plus [package vsatisfies [package present Tcl] 8.5]
tcltest::testConstraint tcl84plus [package vsatisfies [package present Tcl] 8.4]

tcltest::testConstraint tcl85 \
    [expr {
	    [tcltest::testConstraint tcl85plus] &&
	   ![tcltest::testConstraint tcl86plus]
    }]

tcltest::testConstraint tcl84 \
    [expr {
	    [tcltest::testConstraint tcl84plus] &&
	   ![tcltest::testConstraint tcl85plus]
    }]

# -------------------------------------------------------------------------
