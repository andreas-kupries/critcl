# -*- tcl -*-
# -------------------------------------------------------------------------
# critcl_cconst.test
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

    # Note: The next command does not influence the standard argument-
    # and result-types.
    critcl::config lines 0
}

# -------------------------------------------------------------------------
## cconst syntax

test critcl-cconst-1.0 {cconst, wrong args, not enough} -setup {
} -body {
    critcl::cconst
} -returnCodes error -result {wrong # args: should be "critcl::cconst name rtype rvalue"}

test critcl-cconst-1.1 {cconst, wrong args, not enough} -setup {
} -body {
    critcl::cconst N
} -returnCodes error -result {wrong # args: should be "critcl::cconst name rtype rvalue"}

test critcl-cconst-1.2 {cconst, wrong args, not enough} -setup {
} -body {
    critcl::cconst N T
} -returnCodes error -result {wrong # args: should be "critcl::cconst name rtype rvalue"}

test critcl-cconst-1.3 {cconst, wrong args, too many} -setup {
} -body {
    critcl::cconst N T R X
} -returnCodes error -result {wrong # args: should be "critcl::cconst name rtype rvalue"}

test critcl-cconst-1.4 {cconst, bad result type (void)} -setup {
} -body {
    critcl::cconst N void T
} -returnCodes error -result {Constants cannot be of type "void"}

# -------------------------------------------------------------------------
## Go through the various knobs we can use to configure the definition and output

test critcl-cconst-2.0 {cconst, bool, fixed value} -body {
    get critcl::cconst alpha bool 1
} -result [viewFile [localPath test/cconst/2.0]]

test critcl-cconst-2.1 {cconst, bool, define} -body {
    get critcl::cconst alpha bool FOO
} -result [viewFile [localPath test/cconst/2.1]]

test critcl-cconst-2.2 {cconst, bool, function} -body {
    get critcl::cconst alpha bool foo()
} -result [viewFile [localPath test/cconst/2.2]]

test critcl-cconst-2.3 {cconst, namespaced name} -body {
    get critcl::cconst the::alpha bool 0
} -result [viewFile [localPath test/cconst/2.3]]

# -------------------------------------------------------------------------
testsuiteCleanup

# Local variables:
# mode: tcl
# indent-tabs-mode: nil
# End:
