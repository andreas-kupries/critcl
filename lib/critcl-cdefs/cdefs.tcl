# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the per-file C definitions.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::cache  ;# Access to result cache.
package require critcl::gopt   ;# Access to global options.
package require critcl::common ;# General critcl utilities.
package require critcl::data   ;# Access to data files.
package require critcl::meta   ;# Management of teapot meta data.
package require critcl::uuid   ;# Digesting, change detection.
package require critcl::scan   ;# Static Tcl file scanner.
package require critcl::tags   ;# Management of indicator flags.
package require debug          ;# debug narrative

package provide critcl::cdefs 4

namespace eval ::critcl::cdefs {
    namespace export clear code defs flags func-begin func-cdata \
	func-delete func-done hdrs init ldflags libs objs preload \
	srcs tcls usetcl usetk code? edecls? flags? funcs? hdrs? \
	inits? ldflags? libs? objs? preload? srcs? tcls? usetcl? \
	usetk? has-const has-code initialize complete on-clear \
	system-include-paths system-lib-paths
    namespace ensemble create
}

debug level  critcl/cdefs
debug prefix critcl/cdefs {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::cdefs::on-clear {cmd} {
    debug.critcl/cdefs {}
    variable onclear
    lappend  onclear $cmd
    return
}

proc ::critcl::cdefs::initialize {context} {
    if {[tags has $context initialized]} return
    debug.critcl/cdefs {}

    # Initialize the system meta data.
    # User meta data auto-initializes on 1st write.

    meta assign $context platform    [list [TeapotPlatform]]
    meta assign $context build::date [list [common today]]

    # Statically scan the context (file) for dependencies and such,
    # should it exist (Bracket code for example does not). This will
    # also initialize user meta data, if there are any.
    if {[file exists $context]} {
	scan-dependencies $context $context provide
    }

    tags set $context initialized
    return
}

proc ::critcl::cdefs::code {context code} {
    debug.critcl/cdefs {}
    variable fragments
    variable block
    variable defs
    initialize $context

    set digest [uuid add $context .ccode $code]

    dict lappend fragments $context $digest
    dict lappend defs      $context $digest
    dict set     block     $context $digest $code
    return
}

proc ::critcl::cdefs::defs {context defines {namespace "::"}} {
    debug.critcl/cdefs {}
    if {![llength $defines]} return
    variable const
    initialize $context

    uuid add $context .cdefines [list $defines $namespace]

    foreach def $defines {
	# Note: The <def>'s are glob patterns.
	dict set const $context $def $namespace
    }
    return
}

proc ::critcl::cdefs::flags {context words} {
    debug.critcl/cdefs {}
    if {![llength $words]} return
    variable cflags
    initialize $context

    uuid add $context .cflags $words

    foreach flag $words {
	dict lappend cflags $context $flag
    }
    return
}

proc ::critcl::cdefs::func-begin {context tclname cname details} {
    debug.critcl/cdefs {}
    variable functions
    set digest [uuid add $context .function [list $tclname $details]]

    dict lappend functions $context $cname
    return $digest
}

proc ::critcl::cdefs::func-cdata {context cname cdata} {
    debug.critcl/cdefs {}
    variable funcdata
    dict set funcdata $context $cname $cdata
    return
}

proc ::critcl::cdefs::func-delete {context cname delproc} {
    debug.critcl/cdefs {}
    variable fundelete
    dict set fundelete $context $cname $delproc
    return
}

proc ::critcl::cdefs::func-done {context digest code} {
    debug.critcl/cdefs {}
    variable fragments
    variable block

    dict lappend fragments $context $digest
    dict lappend block     $context $code
    return
}

proc ::critcl::cdefs::hdrs {context words} {
    debug.critcl/cdefs {}
    # Accept: -Ipath, path/to/header-file
    FlagsAndPatterns $context cheaders $words -I
    return
}

proc ::critcl::cdefs::init {context code decl} {
    debug.critcl/cdefs {}
    variable initc
    variable edecls
    initialize $context

    uuid add $context .cinit [list $code $edecls]

    dict append initc  $context $code   \n
    dict append edecls $context $edecls \n
    return
}

proc ::critcl::cdefs::ldflags {context words} {
    debug.critcl/cdefs {}
    if {![llength $words]} return
    variable ldflags
    initialize $context

    uuid add $context .ldflags $words

    # Note: Flag may come with and without a -Wl, prefix.
    # We canonicalize this here to always have a -Wl, prefix.
    # This is done by stripping any such prefixes off and then
    # adding it back ourselves.

    foreach flag $words {
	regsub -all {^-Wl,} $flag {} flag
	dict lappend ldflags $context -Wl,$flag
    }
    return
}

proc ::critcl::cdefs::libs {context words} {
    debug.critcl/cdefs {}
    # Accept: -Lpath, -lname, -l:name, path/to/lib-file
    FlagsAndPatterns $context clibraries $words {-L -l}
    return
}

proc ::critcl::cdefs::objs {context words} {
    debug.critcl/cdefs {}
    # words = list (glob-pattern...) = list (file...)
    if {![llength $words]} return
    variable cobjects
    initialize $context

    uuid add $context .cobjects $words

    set base [file dirname $context]
    foreach pattern $words {
	foreach path [common expand-glob $base $pattern] {
	    # XXX TODO: reject non-file|unreadable paths.

	    # Companion C object file content technically affects
	    # binary. Practically this is only used by the critcl
	    # application to link the package object files with the
	    # bracketing library.
	    #uuid add $context .cobject.$path [common cat $path]
	    dict lappend cobjects $context $path
	}
    }
    return
}

proc ::critcl::cdefs::preload {context words} {
    debug.critcl/cdefs {}
    if {![llength $words]} return
    variable preload
    initialize $context

    uuid add $context .preload $words

    foreach lib $words {
	dict lappend preload $context $lib
    }
    return
}

proc ::critcl::cdefs::srcs {context words} {
    debug.critcl/cdefs {}
    # words = list (glob-pattern...) = list (file...)
    if {![llength $words]} return
    variable csources
    initialize $context

    uuid add $context .csources $words

    set base [file dirname $context]
    foreach pattern $words {
	foreach path [common expand-glob $base $pattern] {
	    # Note: This implicitly rejects all paths which are not
	    # readable, nor files.

	    # Companion C file content affects binary.
	    uuid add $context .csources.$path [common cat $path]
	    dict lappend csources $context $path
	}
    }
    return
}

proc ::critcl::cdefs::tcls {context words} {
    debug.critcl/cdefs {}
    # words = list (glob-pattern...) = list (file...)
    if {![llength $words]} return
    variable tsources
    initialize $context

    # Note: The companion Tcl sources (count, order, content) have no bearing
    #       on the binary. Hence no touching of the uuid system here.

    set base [file dirname $context]
    foreach pattern $words {
	foreach path [common expand-glob $base $pattern] {
	    # The scan implicitly rejects paths which are not readable, nor files.
	    dict lappend tsources $context $path
	    scan-dependencies $context $path
	}
    }
    return
}

proc ::critcl::cdefs::usetcl {context version} {
    debug.critcl/cdefs {}
    variable mintcl
    initialize $context

    # This is also a dependency we have to record in the meta data.
    # A 'package require' is not needed. This can be inside of the
    # generated and loaded C code.

    dict set mintcl $context $version
    uuid add       $context .mintcl $version
    meta require   $context [list Tcl $version]
    return
}

proc ::critcl::cdefs::usetk {context} {
    debug.critcl/cdefs {}
    variable usetk
    initialize $context

    # This is also a dependency we have to record in the meta data.
    # A 'package require' is not needed. This can be inside of the
    # generated and loaded C code.

    dict set usetk $context 1
    uuid add       $context .usetk 1
    meta require   $context Tk
    return
}

proc ::critcl::cdefs::code? {context {mode all}} {
    debug.critcl/cdefs {}
    set sep [common separator]
    set code {}
    set block [Get $context block]
    set mode [expr {$mode eq "all" ? "fragments" : "defs"}]
    foreach hash [Get $context $mode] {
	append code $sep \n [dict get $block $hash]
    }
    return $code
}

proc ::critcl::cdefs::edecls? {context} {
    debug.critcl/cdefs {}
    Get $context edecls
}

proc ::critcl::cdefs::flags? {context} {
    debug.critcl/cdefs {}
    Get $context flags
}

proc ::critcl::cdefs::funcs? {context} {
    debug.critcl/cdefs {}
    Get $context functions
}

proc ::critcl::cdefs::hdrs? {context} {
    debug.critcl/cdefs {}
    Get $context cheaders
}

proc ::critcl::cdefs::inits? {context} {
    debug.critcl/cdefs {}
    Get $context initc
}

proc ::critcl::cdefs::ldflags? {context} {
    debug.critcl/cdefs {}
    Get $context ldflags
}

proc ::critcl::cdefs::libs? {context} {
    debug.critcl/cdefs {}
    Get $context clibraries
}

proc ::critcl::cdefs::objs? {context} {
    debug.critcl/cdefs {}
    Get $context cobjects
}

proc ::critcl::cdefs::preload? {context} {
    debug.critcl/cdefs {}
    Get $context preload
}

proc ::critcl::cdefs::srcs? {context} {
    debug.critcl/cdefs {}
    Get $context csources
}

proc ::critcl::cdefs::tcls? {context} {
    debug.critcl/cdefs {}
    Get $context tsources
}

proc ::critcl::cdefs::usetcl? {context} {
    debug.critcl/cdefs {}
    set required [Get $context mintcl 8.4]
    foreach version [data available-tcl] {
	if {[package vsatisfies $version $required]} {
	    return $version
	}
    }
    return $required
}

proc ::critcl::cdefs::usetk? {context} {
    debug.critcl/cdefs {}
    Get $context tk 1
}

proc ::critcl::cdefs::has-const {context} {
    debug.critcl/cdefs {}
    Has $context const
}

proc ::critcl::cdefs::has-code {context} {
    debug.critcl/cdefs {}
    Has $context fragments
}

proc ::critcl::cdefs::system-lib-paths {context} {
    debug.critcl/cdefs {}
    set paths {}
    set has   {}

    # critcl -L options.
    foreach dir [gopt get L] {
	+Path has paths $dir
    }

    # XXX NOTE Use critcl::clibraries?
    return $paths
}

proc ::critcl::cdefs::system-include-paths {context} {
    debug.critcl/cdefs {}
    set paths {}
    set has   {}

    # critcl -I options.
    foreach dir [gopt get I] {
	+Path has paths $dir
    }

    # The result cache is a source of header files too (stubs tables,
    # and other generated files).
    +Path has paths [cache get]

    # critcl::cheaders
    foreach flag [hdrs? $context] {
	if {![string match "-*" $flag]} {
	    # flag = normalized absolute path to a header file.
	    # Transform into a directory reference.
	    set dir [file dirname $flag]
	} else {
	    # Chop leading -I
	    set dir [string range $flag 2 end]
	}

	+Path has paths $dir
    }

    return $paths
}

proc ::critcl::cdefs::clear {context} {
    debug.critcl/cdefs {}
    variable block      ; dict unset block      $context
    variable cflags     ; dict unset cflags     $context
    variable cheaders   ; dict unset cheaders   $context
    variable clibraries ; dict unset clibraries $context
    variable cobjects   ; dict unset cobjects   $context
    variable const      ; dict unset const      $context
    variable csources   ; dict unset csources   $context
    variable defs       ; dict unset defs       $context
    variable edecls     ; dict unset edecls     $context
    variable fragments  ; dict unset fragments  $context
    variable functions  ; dict unset functions  $context
    variable funcdata   ; dict unset funcdata   $context
    variable fundelete  ; dict unset fundelete  $context
    variable initc      ; dict unset initc      $context
    variable ldflags    ; dict unset ldflags    $context
    variable mintcl     ; dict unset mintcl     $context
    variable preload    ; dict unset preload    $context
    variable tk         ; dict unset tk         $context
    variable tsources   ; dict unset tsources   $context

    # Fixed hooks (dependent databases)
    meta clear $context
    uuid clear $context

    # Unwanted tags must be removed explicitly.  Note that clearing
    # this database happens after the referenced file is build, so we
    # can drop the initialization status (and the 'failed' build
    # status appears).
    tags unset      $context debug-memory
    tags unset      $context debug-symbols
    tags unset      $context initialized

    # Invoke the registered hooks.
    variable onclear
    foreach cmd $onclear {
	uplevel #0 $cmd [list $context]
    }
    return
}

proc ::critcl::cdefs::complete {context mode destination initname defines} {
    debug.critcl/cdefs {}
    # mode in stubs, !stubs

    set stubs [expr {$mode eq "stubs"}]
    set fd    [open $destination w]

    CommonHeading $fd $context
    TkHeading     $fd
    CodeBlocks    $fd $context
    SetupTclStubs $fd $context $stubs  
    SetupTkStubs  $fd $context
    SetupTclInit  $fd $context $initname
    SetupTkInit   $fd $context
    SetupUserInit $fd $context
    ExportDefines $fd $context $defines
    CommandSetup  $fd $context
    CommonFooter  $fd

    close $fd
    return $destination
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::cdefs {
    # Per-file (context) databases of C definitions.

    variable block      {} ;# dict (<context> -> <hash> -> C-code)    | ccode
    variable cflags     {} ;# dict (<context> -> list (flag...))      | cflags
    variable cheaders   {} ;# dict (<context> -> list (flag|file...)) | cheaders
    variable clibraries {} ;# dict (<context> -> list (flag|file...)) | clibraries
    variable cobjects   {} ;# dict (<context> -> list (file...))      | cobjects
    variable const      {} ;# dict (<context> -> <def> -> namespace)  * cdefines
    variable csources   {} ;# dict (<context> -> list (file...))      | csources
    variable defs       {} ;# dict (<context> -> list (hash...))      * ccode
    variable edecls     {} ;# dict (<context> -> C-code)              | cinit
    variable fragments  {} ;# dict (<context> -> list (hash...))      | ccode    ccommand cproc
    variable functions  {} ;# dict (<context> -> list (C-name...))    |          ccommand cproc
    variable funcdata   {} ;# dict (<context> -> cname -> cdata)      |          ccommand
    variable fundelete  {} ;# dict (<context> -> cname -> delfunc)    |          ccommand
    variable initc      {} ;# dict (<context> -> C-code)              | cinit
    variable ldflags    {} ;# dict (<context> -> list (flag...))      | ldflags
    variable mintcl     {} ;# dict (<context> -> version)             | tcl
    variable preload    {} ;# dict (<context> -> list (libname...))   | preload
    variable tk         {} ;# dict (<context> -> bool|presence)       | tk
    variable tsources   {} ;# dict (<context> -> list (file...))      | tsources

    #	tsources	- List. The companion tcl sources for <file>.
    #	cheaders	- List. The companion C header files for <file>.
    #	csources	- List. The companion C sources for <file>.
    #	clibraries	- List. Companion libraries to link.
    #	cflags		- List. Additional flags to provide to the compile step.
    #	ldflags		- List. Additional flags to provide to the link step.
    #	initc		- String. Initialization code for Foo_Init(), "critcl::cinit"
    #	edecls		- String. Declarations of externals needed by Foo_Init(), "critcl::cinit"
    #	functions	- List. Collected function names.
    #	fragments	- List. Hashes of the collected C source bodies (functions, and unnamed code).
    #	block		- Dictionary. Maps the hashes (see 'fragments') to the actual C sources.
    #	defs		- List. Hashes of the collected C source bodies (only unnamed code), for extraction of defines.
    #	const		- Dictionary. Maps the names of defines to the namespace the associated variables will be put into.
    #	mintcl		- String. Minimum version of Tcl required by the package.
    #	preload		- List. Names of all libraries to load
    #			  before the package library. This
    #			  information is used only by mode
    #			  'generate package'. This means that
    #			  packages with preload can't be used
    #			  in mode 'compile & run'.

    # List of command prefixes to run when invoking method "clear". I.e. database hooks.
    # Commands are called with relevant reference (1 arg).
    variable onclear {}

    namespace import ::critcl::cache
    namespace import ::critcl::common
    namespace import ::critcl::data
    namespace import ::critcl::gopt
    namespace import ::critcl::meta
    namespace import ::critcl::uuid
    namespace import ::critcl::tags
    namespace import ::critcl::scan-dependencies
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::cdefs::CommonHeading {fd context} {
    debug.critcl/cdefs {}
    set api  [tags get $context apiprefix]
    set file $context
    # Boilerplate header.
    puts $fd [subst [common cat [data cfile header.c]]]
    #         ^=> file, api
    return
}

proc ::critcl::cdefs::CommonFooter {fd} {
    debug.critcl/cdefs {}
    # Complete the trailer and be done.
    puts  $fd [common cat [data cfile pkginitend.c]]
    return
}

proc ::critcl::cdefs::CodeBlocks {fd context} {
    debug.critcl/cdefs {}
    puts $fd [code? $context]
    puts $fd [common separator]
    return
}

proc ::critcl::cdefs::TkHeading {fd context} {
    debug.critcl/cdefs {}
    # Make Tk available, if requested
    if {![cdefs::usetk? $context]} return
    puts $fd "\n#include \"tk.h\""
    return
}

proc ::critcl::cdefs::SetupTclInit {fd context ininame} {
    debug.critcl/cdefs {}
    set ext [cdefs::edecls? $context]
    puts $fd [subst [common cat [data cfile pkginit.c]]]
    #         ^=> ext, ininame
    # This ends in the middle of the FOO_Init() function, leaving it
    # incomplete.
    return
}

proc ::critcl::cdefs::SetupTkInit {fd context} {
    debug.critcl/cdefs {}
    if {![cdefs::usetk? $context]} return
    # From here on we are completing FOO_Init().
    # Tk setup first, if requested. (Tcl is already done).
    puts $fd [common cat [data cfile pkginittk.c]]
    return
}

proc ::critcl::cdefs::SetupUserInit {fd context} {
    debug.critcl/cdefs {}
    # User specified initialization code.
    puts $fd "[cdefs::init? $context] "
    return
}

proc ::critcl::cdefs::SetupTclStubs {fd context placestubs} {
    debug.critcl/cdefs {}
    set mintcl [usetcl? $context]

    if {$placestubs} {
	# Put full stubs definitions into the code, which can be
	# either the bracket generated for a -pkg, or the package
	# itself, build in mode "compile & run".
	set stubs     [data tcl-decls      $mintcl]
	set platstubs [data tcl-plat-decls $mintcl]
	puts -nonewline $fd [subst [common cat [data cfile stubs.c]]]
	#                    ^=> mintcl, stubs, platstubs
    } else {
	# Declarations only, for linking, in the sub-packages.
	puts -nonewline $fd [subst [common cat [data cfile stubs_e.c]]]
	#                    ^=> mintcl
    }
    return
}

proc ::critcl::cdefs::SetupTkStubs {fd context} {
    debug.critcl/cdefs {}
    if {![cdefs::usetk? $context]} return
    puts -nonewline $fd [common cat [data cfile tkstubs.c]]
    return
}

proc ::critcl::cdefs::CommandSetup {fd context} {
    debug.critcl/cdefs {}
    set cd [Get $context funcdata]
    set dp [Get $context fundelete]

    # Take the collected functions and register them as Tcl commands.
    foreach cname [lsort -dict [funcs? $context]] {
	set fcd [expr {[dict exists $cd $cname] ? [dict get $cd $cname] : "NULL"}]
	set fdp [expr {[dict exists $dp $cname] ? [dict get $dp $cname] : 0}]

	puts $fd "  Tcl_CreateObjCommand(ip, ns_$cname, tcl_$cname, $fcd, $fdp);"
    }
    return
}

proc ::critcl::ExportDefines {fd context defines} {
    debug.critcl/cdefs {}
    # Setup of the variables serving up defined constants, if any
    if {![cdefs::has-const $context]} return
    set map [Get $context const]

    # Generate Tcl_ObjSetVar2 commands exporting the constants
    # (defines and/or enums) and their values as Tcl variables.

    foreach {constname constvalue} $defines {
	if {![NamespaceOfConst $map $constname namespace]} continue

	if {![info exists created($namespace)]} {
	    # we need to force the creation of the namespace
	    # because this code will be run before the user code
	    puts $fd "  Tcl_Eval(ip, \"namespace eval $namespace {}\");"
	    set created($namespace) 1
	}
	set var "Tcl_NewStringObj(\"${namespace}::$constname\", -1)"
	if {$constname eq $constvalue} {
	    # enum - assume integer
	    set constvalue "Tcl_NewIntObj($constvalue)"
	} else {
	    # text or int - force to string
	    set constvalue "Tcl_NewStringObj(\"$constvalue\", -1)"
	}
	puts $fd "  Tcl_ObjSetVar2(ip, $var, NULL, $constvalue, TCL_GLOBAL_ONLY);"
    }

    return
}

proc ::critcl::cdefs::NamespaceOfConst {map constname nsvar} {
    debug.critcl/cdefs {}
    foreach {pattern namespace} $map {
	if {![string match $pattern $constname]} continue
	upvar 1 $nsvar nsresult
	set nsresult $namespace
	return yes
    }
    return no
}

proc ::critcl::cdefs::+Path {hv pv path} {
    debug.critcl/cdefs {}
    upvar 1 $hv has $pv pathlist
    if {[dict exists $has $dir]} continue
    dict set has $dir yes
    lappend pathlist $path
    return
}

proc ::critcl::cdefs::Get {context dbvar {default {}}} {
    debug.critcl/cdefs {}
    variable $dbvar
    upvar 0  $dbvar data
    if {![dict exists $data $context]} { return $default }
    return [[dict get $data $context]
}

proc ::critcl::cdefs::Has {context dbvar} {
    debug.critcl/cdefs {}
    variable $dbvar
    upvar 0  $dbvar data
    return [dict exists $data $context]
}

proc ::critcl::cdefs::FlagsAndPatterns {context dbvar words options} {
    debug.critcl/cdefs {}
    # words = list (flag|glob-pattern...) = list (flag|file...)
    # XXX TODO: options = list of allowed flags
    if {![llength $words]} return
    variable $dbvar
    upvar 0  $dbvar options
    initialize $context

    uuid add $context .$dbvar $words

    set base [file dirname $context]

    # words is intermingled flags (-*) and glob-patterns.  Flags are
    # passed through unchanged. Patterns are expanded.  Contents
    # indirectly affect the binary, and are therefore digested.

    foreach flagOrPattern $words {
	if {[string match "-*" $flagOrPattern]} {
	    # Flag, pass unchanged
	    dict lappend options $context $flagOrPattern
	} else {
	    # Pattern. Expand, pass the found files.
	    foreach path [common expand-glob $base $flagOrPattern] {
		uuid add $context .$dbvar.$path [common cat $path]
		dict lappend options $context $path
	    }
	}
    }
    return
}

proc ::critcl::cdefs::Error {msg args} {
    debug.critcl/cdefs {}
    set code [linsert $args 0 CRITCL CDEFS]
    return -code error -errorcode $code $msg
}

proc ::critcl::cdefs::TeapotPlatform {} {
    debug.critcl/cdefs {}
    # Platform identifier HACK. Most of the data in critcl is based on
    # 'platform::generic'. The TEApot MD however uses
    # 'platform::identify' with its detail information (solaris kernel
    # version, linux glibc version). But, if a cross-compile is
    # running we are SOL, because we have no place to pull the
    # necessary detail from, 'identify' is a purely local operation :(

    # XXX FIXME actual target - go through backend!!
    # XXX FIXME!! Initialization step on first declaration.

    set platform [ccconfig::actual]
    if {[platform::generic] eq $platform} {
	set platform [platform::identify]
    }

    return $platform
}

# # ## ### ##### ######## ############# #####################
## Ready
return
