# -*- tcl -*-
# -------------------------------------------------------------------------
# critcl_ccommand.test
# -------------------------------------------------------------------------

source [file join \
            [file dirname [file join [pwd] [info script]]] \
            testutilities.tcl]

testsNeedTcl     8.4
testsNeedTcltest 2

support {
    useLocal lib/util84/lassign.tcl  lassign84
    useLocal lib/util84/dict.tcl     dict84

    useLocal lib/stubs/container.tcl stubs::container
    useLocal lib/stubs/reader.tcl    stubs::reader
    useLocal lib/stubs/genframe.tcl  stubs::gen

    # Helper procedures
    useLocalFile test/support.tcl
}
testing {
    useLocal lib/critcl/critcl.tcl critcl

    overrides
}

# -------------------------------------------------------------------------
## ccommand syntax

test critcl-ccommand-1.0.6 {ccommand, wrong\#args} -constraints tcl86plus -body {
    critcl::ccommand
} -returnCodes error -result {wrong # args: should be "critcl::ccommand name anames ?arg ...?"}

test critcl-ccommand-1.0.5 {ccommand, wrong\#args} -constraints tcl85 -body {
    critcl::ccommand
} -returnCodes error -result {wrong # args: should be "critcl::ccommand name anames ..."}

test critcl-ccommand-1.0.4 {ccommand, wrong\#args} -constraints tcl84 -body {
    critcl::ccommand
} -returnCodes error -result {wrong # args: should be "critcl::ccommand name anames args"}

# -------------------------------------------------------------------------
## Go through the various knobs we can use to configure the definition and output

test critcl-ccommand-2.0 {ccommand, defaults} -body {
    get critcl::ccommand command {} {
        return TCL_OK;
    }
} -result [viewFile [localPath test/ccommand/2.0]]

test critcl-ccommand-2.1 {ccommand, custom arguments} -body {
    get critcl::ccommand command {CD IP OC OV} {
        return TCL_OK;
    }
} -result [viewFile [localPath test/ccommand/2.1]]

test critcl-ccommand-2.2 {ccommand, -cname (custom C name)} -body {
    get critcl::ccommand snafu {} {
        return TCL_OK;
    } -cname 1
} -result [viewFile [localPath test/ccommand/2.2]]

test critcl-ccommand-2.3 {ccommand, namespaced name, and Tcl vs C} -body {
    get critcl::ccommand the::command+max {} {
        return TCL_OK;
    }
} -result [viewFile [localPath test/ccommand/2.3]]

test critcl-ccommand-2.4 {ccommand, -delproc} -body {
    get critcl::ccommand command {} {
        return TCL_OK;
    } -delproc DELE
} -result [viewFile [localPath test/ccommand/2.0]]

test critcl-ccommand-2.5 {ccommand, -clientdata} -body {
    get critcl::ccommand command {} {
        return TCL_OK;
    } -clientdata ABC
} -result [viewFile [localPath test/ccommand/2.0]]

# -------------------------------------------------------------------------
## Full builds.

test critcl-ccommand-3.0 {ccommand, defaults} -setup setup -body {
    critcl::ccommand command {} { return TCL_OK; }
    critcl::cbuild [the-file]
    inspect v*.c
} -cleanup cleanup -match glob \
    -result [viewFile [localPath test/ccommand/3.0]]

test critcl-ccommand-3.4 {ccommand, -delproc} -setup setup -body {
    critcl::ccommand command {} { return TCL_OK; } -delproc DELE
    critcl::cbuild [the-file]
    inspect v*.c
} -cleanup cleanup -match glob \
    -result [viewFile [localPath test/ccommand/3.4]]

test critcl-ccommand-3.5 {ccommand, -clientdata} -setup setup -body {
    critcl::ccommand command {} { return TCL_OK; } -clientdata ABC
    critcl::cbuild [the-file]
    inspect v*.c
} -cleanup cleanup -match glob \
    -result [viewFile [localPath test/ccommand/3.5]]

# -------------------------------------------------------------------------
testsuiteCleanup

# Local variables:
# mode: tcl
# indent-tabs-mode: nil
# End:
