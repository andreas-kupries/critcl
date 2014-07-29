## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# @mdgen OWNER: Config
# @mdgen OWNER: critcl_c

# CriTcl Core.

package provide critcl 3.1.11

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4 ; # Minimal supported Tcl runtime.
if {[catch {
    package require platform 1.0.2 ; # Determine current platform.
}]} {
    # Fall back to our internal copy (currently at platform 1.0.11
    # equivalent) if the environment does not have the official
    # package.
    package require critcl::platform
}

# Ensure forward compatibility of commands defined in 8.5+.
package require lassign84
package require dict84

catch { interp debug {} -frame 1 }


namespace eval ::critcl {}

# # ## ### ##### ######## ############# #####################

package require critcl::at        ;# Management of #line pragmas.
# API exported through critcl core
# ::critcl::at::
#   caller  - stash caller location, possibly modified (level change, line offset)
#   caller! - format & return caller location, clears stash
#   here    - stash current location
#   here!   - return format & return  current location, clears stash
#   incr*   - modify stashed location (only line number, not file).
#   get     - format, return, and clear stash
#   get*    - format & return stash
package require critcl::api       ;# Management of stubs tables. - XXX Dispatch only.
package require critcl::cache     ;# Result cache access.
package require critcl::cc        ;# Backend using an external CC for building, and linking.
package require critcl::ccconfig  ;# CC configuration database for standard backend.
package require critcl::cdefs     ;# General collection of C definitions.
package require critcl::common    ;# General utility commands.
package require critcl::data      ;# Access to templates and other supporting files.
package require critcl::gopt      ;# Management of global options.
package require critcl::meta      ;# Management of teapot meta data.
package require critcl::tags      ;# Management of indicator flags.
package require critcl::typeconv  ;# Handling cproc data types.
package require critcl::usrconfig ;# Management of user options. - XXX Dispatch only
package require critcl::uuid      ;# UUID generation.
package require critcl::who       ;# Management of current file.

# # ## ### ##### ######## ############# #####################
## Define a few shims for public critcl APIs which are now served by
## the utility packages.

interp alias {} ::critcl::clean_cache    {} ::critcl::cache::clear
interp alias {} ::critcl::fastuuid       {} ::critcl::uuid::fast
interp alias {} ::critcl::lappendlist    {} ::critcl::common::lappendlist

interp alias {} ::critcl::argtype        {} ::critcl::typeconv::arg-def
interp alias {} ::critcl::argtypesupport {} ::critcl::typeconv::arg-set-support
interp alias {} ::critcl::resulttype     {} ::critcl::typeconv::result-def

# XXX FIXME move these into critcl::cc. Allows a backend to provide fake information
# I.e a tcc4tcl backend is compile&run only, no x-compile, no configs, etc ...
interp alias {} ::critcl::readconfig     {} ::critcl::ccconfig::read
interp alias {} ::critcl::setconfig      {} ::critcl::ccconfig::use
interp alias {} ::critcl::showallconfig  {} ::critcl::ccconfig::showall
interp alias {} ::critcl::showconfig     {} ::critcl::ccconfig::show
interp alias {} ::critcl::getconfigvalue {} ::critcl::ccconfig::get
interp alias {} ::critcl::knowntargets   {} ::critcl::ccconfig::known
interp alias {} ::critcl::targetconfig   {} ::critcl::ccconfig::target
interp alias {} ::critcl::targetplatform {} ::critcl::ccconfig::targetplatform
interp alias {} ::critcl::buildplatform  {} ::critcl::ccconfig::buildplatform
interp alias {} ::critcl::actualtarget   {} ::critcl::ccconfig::actual
interp alias {} ::critcl::sharedlibext   {} ::critcl::ccconfig::sharedlibext
interp alias {} ::critcl::crosscheck     {} ::critcl::ccconfig::crosscheck

proc ::critcl::cache {{dir {}}} {
    if {[llength [info level 0]] == 2} {
	cache::def $dir
    }
    return [cache::get]
}

# # ## ### ##### ######## ############# #####################
## 

proc ::critcl::buildrequirement {script} {
    # In regular code this does nothing. It is a marker for
    # the static scanner to change under what key to record
    # the 'package require' found in the script.
    uplevel 1 $script
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Embed C Code

proc ::critcl::ccode {text} {
    set file [CheckEntry]

    lassign [at::header $text] leadoffset text
    append block [at::cpragma $leadoffset -2 $file] $text \n

    cdefs::code $file $text
    return
}

proc ::critcl::ccommand {name anames args} {
    set file [CheckEntry]
    cdefs::initialize $file

    # Basic key for the clientdata and delproc arrays.
    set cname $name[uuid::serial $file]

    if {[llength $args]} {
	set body [lindex $args 0]
	set args [lrange $args 1 end]
    } else {
	set body {}
    }

    set clientdata NULL ;# Default: ClientData expression
    set delproc    NULL ;# Default: Function pointer expression
    set acname     0
    while {[string match "-*" $args]} {
        switch -- [set opt [lindex $args 0]] {
	    -clientdata { set clientdata [lindex $args 1] }
	    -delproc    { set delproc    [lindex $args 1] }
	    -cname      { set acname     [lindex $args 1] }
	    default {
		error "Unknown option $opt, expected one of -clientdata, -cname, or -delproc"
	    }
        }
        set args [lrange $args 2 end]
    }

    # Put body back into args for integration into the MD5 uuid
    # generated for mode compile&run. Bug and fix reported by Peter
    # Spjuth.
    lappend args $body

    if {$acname} {
	BeginCommand static $name $anames $args
	set ns    {}
	set cns   {}
	set key   $cname
	set wname $name
    } else {
	lassign [BeginCommand public $name $anames $args] ns cns name cname
	set key   [string map {:: _} $ns$cname]
	set wname tcl_$cns$cname
    }

    cdefs::func-cdata  $file $cns$cname $clientdata
    cdefs::func-delete $file $cns$cname $delproc

    #set body [join $args]
    if {$body != ""} {
	lappend anames ""
	foreach {cd ip oc ov} $anames break
	if {$cd eq ""} { set cd clientdata }
	if {$ip eq ""} { set ip interp }
	if {$oc eq ""} { set oc objc }
	if {$ov eq ""} { set ov objv }

	set ca "(ClientData $cd, Tcl_Interp *$ip, int $oc, Tcl_Obj *CONST $ov\[])"

	Emitln "static int $wname$ca"
	Emit   \{\n
	lassign [at::header $body] leadoffset body
	if {[gopt::get lines]} {
	    Emit [at::cpragma $leadoffset -2 $file]
	}
	Emit   $body
	Emitln \n\}
    } else {
	# if no body is specified, then $anames is alias for the real cmd proc
	Emitln "#define $wname $anames"
	Emitln "int $anames\(\);"
    }
    EndCommand
    return
}

proc ::critcl::cdata {name data} {
    CheckEntry
    binary scan $data c* bytes ;# split as bytes, not (unicode) chars

    set inittext ""
    set line ""
    foreach x $bytes {
	if {[string length $line] > 70} {
	    append inittext "    " $line \n
	    set line ""
	}
	append line $x ,
    }
    append inittext "    " $line

    set count [llength $bytes]

    set body [subst [common::cat [data::cfile cdata.c]]]
    #               ^=> count, inittext

    # NOTE: The uplevel is needed because otherwise 'ccommand' will
    # not properly determine the caller's namespace.
    uplevel 1 [list ::critcl::ccommand $name {dummy ip objc objv} [at::caller!]$body]
    return $name
}

proc ::critcl::cdefines {defines {namespace "::"}} {
    set file [CheckEntry]
    cdefs::defs $file $defines $namespace
    return
}

proc ::critcl::argoptional {adefs} {
    set optional {}

    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    lappend optional 1
	} else {
	    lappend optional 0
	}
    }

    return $optional
}

proc ::critcl::argdefaults {adefs} {
    set defaults {}

    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    lappend defaults [lindex $a 1]
	}
    }

    return $defaults
}

proc ::critcl::argnames {adefs} {
    set names {}

    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    set a [lindex $a 0]
	}
	lappend names $a
    }

    return $names
}

proc ::critcl::argcnames {adefs {interp ip}} {
    set cnames {}

    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	lappend cnames interp
	set     adefs  [lrange $adefs 2 end]
    }

    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    set a [lindex $a 0]
	}
	lappend cnames _$a
    }

    return $cnames
}

proc ::critcl::argcsignature {adefs} {
    # Construct the signature of the low-level C function.

    set cargs {}

    # If the 1st argument is "Tcl_Interp*", we pass it without
    # counting it as a command argument.

    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	lappend cargs  [lrange $adefs 0 1]
	set     adefs  [lrange $adefs 2 end]
    }

    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    set a [lindex $a 0]
	}
	lappend cargs  "[typeconv::arg-get-arg-type $t] $a"
    }

    return $cargs
}

proc ::critcl::argvardecls {adefs} {
    # Argument variables, destinations for the Tcl -> C conversion.

    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    set result {}
    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    set a [lindex $a 0]
	}
	lappend result "[typeconv::arg-get-var-type $t] _$a;"
    }

    return $result
}

proc ::critcl::argsupport {adefs} {
    # Argument global support, outside/before function.

    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    set has {}

    set result {}
    foreach {t a} $adefs {
	if {[lsearch -exact $has $t] >= 0} continue
	lappend has $t
	lappend result [typeconv::arg-get-support $t]
    }

    return $result
}

proc ::critcl::argconversion {adefs {n 1}} {
    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    set min $n ; # count all non-optional arguments. min required.
    foreach {t a} $adefs {
	if {[llength $a] == 2} continue
	incr min
    }

    set result {}
    set opt 0
    set prefix "    idx_ = $n;\n"

    foreach {t a} $adefs {
	if {[llength $a] == 2} {
	    # Optional argument. Can be first, or later.
	    # For the first the prefix gives us the code to initialize idx_.

	    lassign $a a default

	    set map [list @@ "ov\[idx_\]" @A _$a]
	    set code [string map $map [typeconv::arg-get-conv $t]]

	    set code "${prefix}  if (oc > $min) \{\n$code\n    idx_++;\n  \} else \{\n    _$a = $default;\n  \}"
	    incr min

	    lappend result "  /* ($t $a, optional, default $default) - - -- --- ----- -------- */"
	    lappend result $code
	    lappend result {}
	    set opt 1
	    set prefix ""
	} elseif {$opt} {
	    # Fixed argument, after the optionals.
	    # Main issue: Use idx_ to access the array.
	    # We know that no optionals can follow, only the same.

	    set map [list @@ "ov\[idx_\]" @A _$a]
	    lappend result "  /* ($t $a) - - -- --- ----- -------- */"
	    lappend result [string map $map [typeconv::arg-get-conv $t]]
	    lappend result "  idx_++;"
	    lappend result {}

	} else {
	    # Fixed argument, before any optionals.
	    set map [list @@ "ov\[$n\]" @A _$a]
	    lappend result "  /* ($t $a) - - -- --- ----- -------- */"
	    lappend result [string map $map [typeconv::arg-get-conv $t]]
	    lappend result {}
	    incr n
	    set prefix "    idx_ = $n;\n"
	}
    }

    return $result
}

proc ::critcl::cproc {name adefs rtype {body "#"} args} {
    set file [CheckEntry]
    cdefs::initialize $file

    set acname 0
    set passcd 0
    set aoffset 0
    while {[string match "-*" $args]} {
        switch -- [set opt [lindex $args 0]] {
	    -cname      { set acname  [lindex $args 1] }
	    -pass-cdata { set passcd  [lindex $args 1] }
	    -arg-offset { set aoffset [lindex $args 1] }
	    default {
		error "Unknown option $opt, expected one of -cname, or -pass-cdata"
	    }
        }
        set args [lrange $args 2 end]
    }

    switch -regexp -- [join [argoptional $adefs] {}] {
	^0*$ -
	^0*1+0*$ {
	    # no optional arguments, or a single optional block at the
	    # beginning, middle, or end of the argument list is what
	    # we are able to handle.
	}
	default {
	    # TODO FUTURE: We can handle this, see how cmdr does it
	    # with thresholds.
	    error "Unable to handle multiple segments of optional arguments"
	}
    }

    if {$acname} {
	BeginCommand static $name $adefs $rtype $body
	set ns  {}
	set cns {}
	set wname $name
	set cname c_$name
    } else {
	lassign [BeginCommand public $name $adefs $rtype $body] ns cns name cname
	set wname tcl_$cns$cname
	set cname c_$cns$cname
    }

    set names  [argnames      $adefs]
    set cargs  [argcsignature $adefs]
    set cnames [argcnames     $adefs]

    if {$passcd} {
	set cargs  [linsert $cargs 0 {ClientData clientdata}]
	set cnames [linsert $cnames 0 cd]
    }

    Emit [join [argsupport $adefs] \n]

    # Emit either the low-level function, or, if it wasn't defined
    # here, a reference to the shim we can use.

    if {$body ne "#"} {
	Emit   "static [typeconv::result-get-type $rtype] "
	Emitln "${cname}([join $cargs {, }])"
	Emit   \{\n
	lassign [at::header $body] leadoffset body
	if {[gopt::get lines]} {
	    Emit [at::cpragma $leadoffset -2 $file]
	}
	Emit   $body
	Emitln \n\}
    } else {
	Emitln "#define $cname $name"
    }

    # Construct the shim handling the conversion between Tcl and C
    # realms.

    EmitShimHeader         $wname
    EmitShimVariables      $adefs $rtype
    EmitWrongArgsCheck     $adefs $aoffset
    EmitArgumentConversion $adefs $aoffset
    EmitCall               $cname $cnames $rtype
    EmitShimFooter         $rtype
    EndCommand
    return
}

proc ::critcl::cinit {text edecls} {
    set file [CheckEntry]

    set skip [at::lines $text]
    lassign [at::header $text] leadoffset text
    if {[gopt::get lines]} {
	append initc [at::cpragma $leadoffset -2 $file]
    }
    append initc $text \n

    lassign [at::header $edecls] leadoffset edecls
    if {[gopt::get lines]} {
	incr leadoffset $skip
	append edec [at::cpragma $leadoffset -2 $file]
    }
    append edec $edecls \n

    cdefs::init $initc $edec
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Input and Output control

proc ::critcl::collect {script {slot {}}} {
    collect_begin $slot
    uplevel 1 $script
    return [collect_end]
}

proc ::critcl::collect_begin {{slot {}}} {
    # Divert the collection of code fragments to slot
    # (output control). Stack on any previous diversion.

    if {$slot eq {}} {
	set slot MEMORY[who::depth]
    }

    # The added schema-prefix prevents collisions between our slot
    # names and the paths used for regular .critcl files.
    who::push critcl://$slot
    return
}

proc ::critcl::collect_end {} {
    # Stop last diversion, and return the collected information as
    # single string of C code.

    # Ensure that a diversion is actually open.
    if {![who::depth]} {
	return -code error \
	    -errorcode {CRITCL COLLECT MISMATCH} \
	    "collect_end mismatch, no diversions active"
    }

    set slot  [who::pop]
    set block [cdefs::code? $slot]
    # Drop all the collected data. Note how anything other than the C
    # code fragments is lost, and how cbuild results are removed
    # also. These do not belong anyway.
    cdefs::clear $slot
    return $block
}

proc ::critcl::include {path} {
    # Include a header or other C file into the current code.
    msg -nonewline " (include <$path>)"
    ccode "#include <$path>"
}

proc ::critcl::make {path contents} {
    # Generate a header or other C file for pickup by other parts of
    # the current package. Stored in the cache dir, making it local.
    file mkdir [cache]
    set cname [file join [cache] $path]

    set c [open $cname.[pid] w]
    puts -nonewline $c $contents\n\n
    close $c
    file rename -force $cname.[pid] $cname

    return $path
}

proc ::critcl::source {path} {
    # Source a critcl file in the context of the current file,
    # i.e. [who::is]. Enables the factorization of a large critcl
    # file into smaller, easier to read pieces.
    set file [CheckEntry]

    msg -nonewline " (importing $path)"

    set undivert 0
    if {![who::depth]} {
	# critcl::source is recording the critcl commands in the
	# context of the toplevel file which started the chain the
	# critcl::source. So why are we twiddling with the diversion
	# state?
	#
	# The condition above tells us that we are in the first
	# non-diverted critcl::source called by the context. [who::is]
	# returns that context. Due to our use of regular 'source' (*)
	# during its execution [who::is] would return the sourced file as
	# context. Wrong. Our fix for this is to perform, essentially,
	# an anti-diversion. Saving [who::is] as diversion, forces it to
	# return the proper value during the whole sourcing.
	#
	# And if the critcl::source is run in an already diverted
	# context then the changes to [info script] by 'source' do not
	# matter, making an anti-diversion unnecessary.
	#
	# Diversions inside of 'source' will work as usual, given
	# their nesting nature.
	#
	# (Ad *) And we use 'source' as only this ensures proper
	# collection of [info frame] location information.

	who::push [who::is]
	set undivert 1
    }

    set base [file dirname $file]
    foreach f [common::expand-glob $base $path] {
	at::script $f
	uplevel #0 [list ::source $f]
	at::script {}
    }

    if {$undivert} who::pop
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Control & Interface

proc ::critcl::owns {args} {}

proc ::critcl::cheaders {args} {
    set file [CheckEntry]
    cdefs::hdrs $file $args
    return
}

proc ::critcl::csources {args} {
    set file [CheckEntry]
    cdefs::srcs $file $args
    return
}

proc ::critcl::clibraries {args} {
    set file [CheckEntry]
    cdefs::libs $file $args
    return
}

proc ::critcl::cobjects {args} {
    set file [CheckEntry]
    cdefs::objs $file $args
    return
}

proc ::critcl::tsources {args} {
    set file [CheckEntry]
    cdefs::tcls $file $args
    return
}

proc ::critcl::cflags {args} {
    set file [CheckEntry]
    cdefs::flags $file $args 0
    return
}

proc ::critcl::ldflags {args} {
    set file [CheckEntry]
    cdefs::ldflags $file $args
    return
}

proc ::critcl::framework {args} {
    CheckEntry

    # XXX FIXME actual target - go through backend?!

    # Check if we are building for OSX and ignore the command if we
    # are not. Our usage of "ccconfig::actual" means that we allow for
    # a cross-compilation environment to OS X as well.
    if {![string match "macosx*" [ccconfig::actual]]} return

    foreach arg $args {
	# if an arg contains a slash it must be a framework path
	if {[string first / $arg] == -1} {
	    ldflags -framework $arg
	} else {
	    cflags  -F$arg
	    ldflags -F$arg
	}
    }
    return
}

proc ::critcl::tcl {version} {
    set file [CheckEntry]
    cdefs::usetcl $file $version
    return
}

proc ::critcl::tk {} {
    set file [CheckEntry]
    cdefs::usetk $file
    return
}

# Register a shared library for pre-loading - this will eventually be
# redundant when TIP #239 is widely available
proc ::critcl::preload {args} {
    set file [CheckEntry]
    cdefs::preload $file $args
    return
}

proc ::critcl::license {who args} {
    set file [CheckEntry]
    cdefs::initialize $file
    meta::license $file $who $args
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: meta data (teapot)

proc ::critcl::description {text} {
    set file [CheckEntry]
    cdefs::initialize $file
    meta::description $file $text
    return
}

proc ::critcl::summary {text} {
    set file [CheckEntry]
    cdefs::initialize $file
    meta::summary $file $text
    return
}

proc ::critcl::subject {args} {
    set file [CheckEntry]
    cdefs::initialize $file
    meta::subject $file $args
    return
}

proc ::critcl::meta {key args} {
    set file [CheckEntry]
    cdefs::initialize $file
    meta::general $file $key $args
    return
}

proc ::critcl::meta? {key} {
    set file [CheckEntry]
    cdefs::initialize $file
    return [meta::get $file $key]
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: user configuration options.

proc ::critcl::userconfig {cmd args} {
    set file [CheckEntry]
    if {![llength [info commands ::critcl::usrconfig::c_$cmd]]} {
	return -code error "Unknown method \"$cmd\""
    }

    # Dispatch
    return [eval [linsert $args 0 ::critcl::usrconfig::c_$cmd $file]]
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: API (stubs) management

proc ::critcl::api {cmd args} {
    set file [CheckEntry]
    if {![llength [info commands ::critcl::api::c_$cmd]]} {
	return -code error "Unknown method \"$cmd\""
    }

    # Dispatch
    return [eval [linsert $args 0 ::critcl::api::c_$cmd $file]]
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Introspection

proc ::critcl::check {args} {
    set file [CheckEntry 0]

    # Syntax:
    # (1) check <code>
    # (2) check <label> <code>

    switch -exact -- [llength $args] {
	1 {
	    set label Checking
	    set code  [lindex $args 0]
	}
	2 {
	    lassign $args label code
	}
	default {
	    return -code error "wrong#args: Expected ?label? code"
	}
    }

    set ok [cc::build-direct $file $label $code]
    return $ok
}

proc ::critcl::checklink {args} {
    set file [CheckEntry 0]

    # Syntax:
    # (1) check <code>
    # (2) check <label> <code>

    switch -exact -- [llength $args] {
	1 {
	    set label Checking
	    set code  [lindex $args 0]
	}
	2 {
	    lassign $args label code
	}
	default {
	    return -code error "wrong#args: Expected ?label? code"
	}
    }

    set ok [cc::link-direct $file $label $code]
    return $ok
}

proc ::critcl::compiled {} {
    CheckEntry 1
    return 0
}

proc ::critcl::compiling {} {
    CheckEntry 0
    # Check that we can indeed run a compiler
    # Should only need to do this if we have to compile the code?
    return [cc::has-compiler]
}

proc ::critcl::done {} {
    return [tags::has [SkipIgnored [who::is] 1] failed]
}

proc ::critcl::failed {} {
    SkipIgnored [who::is] 0
    return [cbuild [who::is] 0]
}

proc ::critcl::load {} {
    SkipIgnored [who::is] 1
    return [expr {![cbuild [who::is]]}]
}

# # ## ### ##### ######## ############# #####################
## Default error behaviour

proc ::critcl::error {msg} {
    return -code error $msg
}

# # ## ### ##### ######## ############# #####################
## Default message behaviour

proc ::critcl::msg {args} {
    # ignore message (compile & run)
}

# # ## ### ##### ######## ############# #####################
## Default print behaviour

proc ::critcl::print {args} {
    # API same as for builtin ::puts. Use as is.
    return [eval [linsert $args 0 ::puts]]
}

# # ## ### ##### ######## ############# #####################
## Runtime support for a special situation.

## A .critcl file is allowed to use itself as a Tcl companion file
## (See "tsources"). This means that the .critcl is plainly "source"d
## when the package is loaded after building (whether in "Load" (for
## compile&run), or by the ifneeded script when the package is
## prebuilt). At that time we are not allowed to process the critcl
## commands again. We must ignore them. For some commands we have
## generate best-effort guesses as to their return values ("failed",
## "done", "load", "check(link)", "compiled", and "compiling").
#
## The two commands below handle this, one to set the ignore indicator
## for a file, the other test for it. This latter is used by all the
## relevant API commands.

proc ::critcl::Ignore {f} {
    tags::set [file normalize $f] ignore
    return
}

proc ::critcl::SkipIgnored {f {result {}}} {
    # File is marked to be ignored. Force the caller to return with
    # specified fake result.
    if {[tags::has $f ignore]} { return -code return $result }
    return $f
}

proc ::critcl::CheckEntry {{result {}}} {
    set file [who::is]

    # Inlined SkipIgnored...
    # File is marked to be ignored. Force the caller to return with
    # specified fake result.
    if {[tags::has $file ignore]} { return -code return $result }

    # Inlined AbortWhenCalledAfterBuild, no separate definition anymore.
    # Inlined [done]. Simplified, always called after SkipIgnored.
    # When not done simply return the file|ref to operate on.
    if {![tags::has $file failed]} { return $file }

    # Fail case. The file is marked as done, yet now we got another
    # definition, and thus have to error out.
    set cloc {}
    if {![catch {
	array set loc [info frame -2]
    } msg]} {
	if {$loc(type) eq "source"} {
	    set cloc "@$loc(file):$loc(line)"
	} else {
	    set cloc " ([array get loc])"
	}
    } ;#else { set cloc " ($msg)" }
    return -code error \
	-errorcode {CRITCL DONE} \
	"[lindex [info level -1] 0]$cloc: Illegal attempt to define C code in [who::is] after it was built."
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Build Management

proc ::critcl::config {option {newvalue {}}} {
    if {[llength [info level 0]] < 3} {
	return [gopt::get $option]
    }
    return [gopt::set $option $newvalue]
}

proc ::critcl::debug {args} {
    set file [CheckEntry]
    # The information is stored in tags.
    foreach flag $args {
	switch -exact -- $flag {
	    all {
		tags::set $file debug-memory
		tags::set $file debug-symbols
	    }
	    memory {
		tags::set $file debug-memory
	    }
	    symbols {
		tags::set $file debug-symbols
	    }
	    default {
		return -code error \
		    -errorcode {CRITCL DEBUG INVALID} \
		    "Invalid flag \"$flag\", expected one of all, memory, or symbols."
	    }
	}
    }
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Build Configuration
# read toolchain information from config file

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Application

# The regular commands used by the application, defined in other
# sections of the package are:
#
# C critcl::cache
# C critcl::ccode
# C critcl::chooseconfig
# C critcl::cinit
# C critcl::clean_cache
# C critcl::clibraries
# C critcl::cobjects
# C critcl::config I, lines, force, keepsrc, combine
# C critcl::debug
# C critcl::error               | App overrides our implementation.
# C critcl::getconfigvalue
# C critcl::lappendlist
# C critcl::ldflags
# C critcl::preload
# C critcl::readconfig
# C critcl::setconfig
# C critcl::showallconfig
# C critcl::showconfig

# Build Variants:
# - cbuild         - autoindex  - build, link, load (c&run) | drop results
# - cbuild-pkgpart - app-critcl - build                     | keep results
# - cbuild-pkgmain - app-critcl - build, link               | keep results

proc ::critcl::cbuild {file {load yes}} {
    # Compile & Run mode.
    # Critcl App - Cache Prefill mode.

    # Build, Link, and Load.
    # Do not keep results.
    # Check for a cached shlib (avoid recollection, define processing).

    set flags [api::complete $file]
    # XXX FIXME: api::complete flags information is not used.

    # Basename for all generated files (.c, .o, .so)
    set base     [file normalize [cache::get ${prefix}_[uuid::get $file]]]
    set initname [DetermineInitName $file]

    cc::build-immediate-begin ccstate $file $v::prefix $base $initname

    if {[cc::build-immediate-required ccstate]} {
	if {[cdefs::has-const $file]} {
	    set defines [cc::extract-const $file]
	} else {
	    set defines {}
	}
	cc::build-immediate ccstate \
	    [cdefs::complete $file stubs $base.c $initname $defines]
    }

    cc::build-immediate-complete ccstate $load

    # Build for the file completed, drop all the collected information
    # we now do not need anymore.
    cdefs::clear $file

    # XXX FIXME how to report build errors when called via [unknown].

    return [tags::get $file failed]
}

proc ::critcl::DetermineInitName {file {wrapped 0}} {
    set ininame [PkgInit $file]

    # Add in the build prefix, if specified. This is done in mode
    # 'generate package', for the pieces, ensuring that the overall
    # initialization function cannot be in conflict with the
    # initialization functions of these same pieces.

    if {$wrapped} {
        set ininame "ns_$ininame"
    }

    return $ininame
}

proc ::critcl::PkgInit {file} {
    # The init function name takes a capitalized prefix from the name
    # of the input file name (alphanumeric prefix, including
    # underscores). This implicitly drops the file extension, as the
    # '.' is not an allowed character.

    # While related to the package name, it can be different,
    # especially if the package name contains :: separators.

    if {$file eq {}} {
	return Stdin
    } else {
	set ininame [file rootname [file tail $file]]
	regsub -all {[^[:alnum:]_]} $ininame {} ininame
	return [string totitle $ininame]
    }
}

## XXX TODO refactor
## These two build commands should be moved into the application,
## using direct calls into the databases and backend to perform their
## work without burdening the core critcl package (except that the work
## on these should help in factoring the backend into better reusable
## pieces).
##

proc ::critcl::cbuild-pkgpart {file} {
    # Critcl App - Prebuild package - Package parts
    # Compile. No Link. No load.
    # Stash results for use by the critcl app.

    # Fast path for known result.
    if {[tags::has $file failed]} {
	return [tags::get $file failed]
    }

    # Complete stubs handling for the file/package.
    # This feeds a number of last-minute C fragments into the system.

    set flags [api::complete $file]
    # XXX FIXME: api::complete flags information is not used.

    # XXX FIXME handling of BuildDefines and CollectEmbedded (= cdefs::complete) in case of cached shlib)

    # Begin with result dict here ... Keeping or not is handled after build/load...
    set result {}
    cc::build-for result $v::prefix $file 1 0

    # Store collected results for pickup through "cresults".
    tags::set $file result $result

    cdefs::clear $file
    return [tags::get $file failed]
}

proc ::critcl::cbuild-pkgmain {} {
    # Critcl App - Prebuild package - Main package bracketing code.
    # Compile. Link. No load.
    # Do not keep results.

    set file [who::is]

    # Fast path for known result.
    if {[tags::has $file failed]} {
	return [tags::get $file failed]
    }

    # Complete stubs handling for the file/package.
    # This feeds a number of last-minute C fragments into the system.

    set flags [api::complete $file]
    # XXX FIXME: api::complete flags information is not used.

    # Begin with result dict here ... Keeping or not is handled after build/load...
    set result {}
    cc::build-for result $v::prefix $file 0 0

    cdefs::clear $file
    return [tags::get $file failed]

}

proc ::critcl::cresults {{file {}}} {
    if {$file eq ""} { set file [who::is] }
    return [tags::get $file result]
}

proc ::critcl::cnothingtodo {f} {
    # We have a build status, so there had been something to do.
    if {[tags::has $f failed]} { return 0 }

    # No critcl definitions at all ?
    if {![tags::has $f initialized]} { return 1 }

    # No C code collected for compilation ?
    if {![cdefs::has-code $f]} { return 1 }

    # Ok, something has to be done.
    return 0
}

proc ::critcl::c++command {tclname class constructors methods} {
    # Build the body of the function to define a new tcl command for
    # the C++ class
    set helpline {}
    set classptr ptr_$tclname
    set comproc "    $class* $classptr;\n"
    append comproc "    switch (objc) \{\n"

    if {![llength $constructors]} {
	set constructors {{}}
    }

    foreach adefs $constructors {
	array set types {}
	set names {}
	set cargs {}
	set cnames {}

	foreach {t n} $adefs {
	    set types($n) $t
	    lappend names $n
	    lappend cnames _$n
	    lappend cargs "$t $n"
	}
	lappend helpline "$tclname pathName [join $names { }]"
	set nargs  [llength $names]
	set ncargs [expr {$nargs + 2}]
	append comproc "        case $ncargs: \{\n"

	if {!$nargs} {
	    append comproc "            $classptr = new $class\();\n"
	} else  {
	    append comproc [ProcessArgs types $names $cnames]
	    append comproc "            $classptr = new $class\([join $cnames {, }]);\n"
	}
	append comproc "            break;\n"
	append comproc "        \}\n"

    }
    append comproc "        default: \{\n"
    append comproc "            Tcl_SetResult(ip, \"wrong # args: should be either [join $helpline { or }]\",TCL_STATIC);\n"
    append comproc "            return TCL_ERROR;\n"
    append comproc "        \}\n"
    append comproc "    \}\n"

    append comproc "    if ( $classptr == NULL ) \{\n"
    append comproc "        Tcl_SetResult(ip, \"Not enough memory to allocate a new $tclname\", TCL_STATIC);\n"
    append comproc "        return TCL_ERROR;\n"
    append comproc "    \}\n"

    append comproc "    Tcl_CreateObjCommand(ip, Tcl_GetString(objv\[1]), cmdproc_$tclname, (ClientData) $classptr, delproc_$tclname);\n"
    append comproc "    return TCL_OK;\n"
    #
    #  Build the body of the c function called when the object is deleted
    #
    set delproc "void delproc_$tclname\(ClientData cd) \{\n"
    append delproc "    if (cd != NULL)\n"
    append delproc "        delete ($class*) cd;\n"
    append delproc "\}\n"

    #
    # Build the body of the function that processes the tcl commands for the class
    #
    set cmdproc "int cmdproc_$tclname\(ClientData cd, Tcl_Interp* ip, int objc, Tcl_Obj *CONST objv\[]) \{\n"
    append cmdproc "    int index;\n"
    append cmdproc "    $class* $classptr = ($class*) cd;\n"

    set rtypes {}
    set tnames {}
    set mnames {}
    set adefs {}
    foreach {rt n a} $methods {
	lappend rtypes $rt
	lappend tnames [lindex $n 0]
	set tmp [lindex $n 1]
	if {$tmp eq ""}  {
	    lappend mnames [lindex $n 0]
	} else {
	    lappend mnames [lindex $n 1]
	}
	lappend adefs $a
    }
    append cmdproc "    static const char* cmds\[]=\{\"[join $tnames {","}]\",NULL\};\n"
    append cmdproc "    if (objc<2) \{\n"
    append cmdproc "       Tcl_WrongNumArgs(ip, 1, objv, \"expecting pathName option\");\n"
    append cmdproc "       return TCL_ERROR;\n"
    append cmdproc "    \}\n\n"
    append cmdproc "    if (Tcl_GetIndexFromObj(ip, objv\[1], cmds, \"option\", TCL_EXACT, &index) != TCL_OK)\n"
    append cmdproc "        return TCL_ERROR;\n"
    append cmdproc "    switch (index) \{\n"

    set ndx 0
    foreach rtype $rtypes tname $tnames mname $mnames adef $adefs {
	array set types {}
	set names {}
	set cargs {}
	set cnames {}

	switch -- $rtype {
	    ok      { set rtype2 "int" }
	    string -
	    dstring -
	    vstring { set rtype2 "char*" }
	    default { set rtype2 $rtype }
	}

	foreach {t n} $adef {
	    set types($n) $t
	    lappend names $n
	    lappend cnames _$n
	    lappend cargs "$t $n"
	}
	set helpline "$tname [join $names { }]"
	set nargs  [llength $names]
	set ncargs [expr {$nargs + 2}]

	append cmdproc "        case $ndx: \{\n"
	append cmdproc "            if (objc==$ncargs) \{\n"
	append cmdproc  [ProcessArgs types $names $cnames]
	append cmdproc "                "
	if {$rtype ne "void"} {
	    append cmdproc "$rtype2 rv = "
	}
	append cmdproc "$classptr->$mname\([join $cnames {, }]);\n"
	append cmdproc "                "
	switch -- $rtype {
	    void     { }
	    ok   { append cmdproc "return rv;" }
	    int  { append cmdproc "Tcl_SetIntObj(Tcl_GetObjResult(ip), rv);" }
	    long { append cmdproc " Tcl_SetLongObj(Tcl_GetObjResult(ip), rv);" }
	    float -
	    double { append cmdproc "Tcl_SetDoubleObj(Tcl_GetObjResult(ip), rv);" }
	    char*  { append cmdproc "Tcl_SetResult(ip, rv, TCL_STATIC);" }
	    string -
	    dstring  { append cmdproc "Tcl_SetResult(ip, rv, TCL_DYNAMIC);" }
	    vstring  { append cmdproc "Tcl_SetResult(ip, rv, TCL_VOLATILE);" }
	    default  { append cmdproc "if (rv == NULL) \{ return TCL_ERROR ; \}\n  Tcl_SetObjResult(ip, rv); Tcl_DecrRefCount(rv);" }
	}
	append cmdproc "\n"
	append cmdproc "                "
	if {$rtype ne "ok"} { append cmdproc "return TCL_OK;\n" }

	append cmdproc "            \} else \{\n"
	append cmdproc "               Tcl_WrongNumArgs(ip, 1, objv, \"$helpline\");\n"
	append cmdproc "               return TCL_ERROR;\n"
	append cmdproc "            \}\n"
	append cmdproc "        \}\n"
	incr ndx
    }
    append cmdproc "    \}\n\}\n"

    # TODO: line pragma fix ?!
    ccode $delproc
    ccode $cmdproc

    # Force the new ccommand to be defined in the caller's namespace
    # instead of improperly in ::critcl.
    namespace eval [uplevel 1 namespace current] \
	[list critcl::ccommand $tclname {dummy ip objc objv} $comproc]

    return
}

proc ::critcl::ProcessArgs {typesArray names cnames}  {
    upvar 1 $typesArray types
    set body ""
    foreach x $names c $cnames {
	set t $types($x)
	switch -- $t {
	    int - long - float - double - char* - Tcl_Obj* {
		append body "            $t $c;\n"
	    }
	    default {
		append body "            void* $c;\n"
	    }
	}
    }
    set n 1
    foreach x $names c $cnames {
	set t $types($x)
	incr n
	switch -- $t {
	    int {
		append body "            if (Tcl_GetIntFromObj(ip, objv\[$n], &$c) != TCL_OK)\n"
		append body "                return TCL_ERROR;\n"
	    }
	    long {
		append body "            if (Tcl_GetLongFromObj(ip, objv\[$n], &$c) != TCL_OK)\n"
		append body "                return TCL_ERROR;\n"
	    }
	    float {
		append body "            \{ double tmp;\n"
		append body "                if (Tcl_GetDoubleFromObj(ip, objv\[$n], &tmp) != TCL_OK)\n"
		append body "                   return TCL_ERROR;\n"
		append body "                $c = (float) tmp;\n"
		append body "            \}\n"
	    }
	    double {
		append body "            if (Tcl_GetDoubleFromObj(ip, objv\[$n], &$c) != TCL_OK)\n"
		append body "                return TCL_ERROR;\n"
	    }
	    char* {
		append body "            $c = Tcl_GetString(objv\[$n]);\n"
	    }
	    default {
		append body "            $c = objv\[$n];\n"
	    }
	}
    }
    return $body
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - cproc conversion helpers.

proc ::critcl::EmitShimHeader {wname} {
    # Function head
    set ca "(ClientData cd, Tcl_Interp *interp, int oc, Tcl_Obj *CONST ov\[])"
    Emitln
    Emitln "static int"
    Emitln "$wname$ca"
    Emitln \{
    return
}

proc ::critcl::EmitShimVariables {adefs rtype} {
    set opt 0
    foreach d [argvardecls $adefs] o [argoptional $adefs] {
	Emitln "  $d"
	if {$o} {set opt 1}
    }
    if {$opt} { Emitln "  int idx_;" }

    # Result variable, source for the C -> Tcl conversion.
    if {$rtype ne "void"} {
	Emit "  [typeconv::result-get-type $rtype] rv;"
    }
    return
}

proc ::critcl::EmitWrongArgsCheck {adefs offset} {
    # Code checking for the correct count of arguments, and generating
    # the proper error if not.

    # A 1st argument matching "Tcl_Interp*" does not count as a user
    # visible command argument.
    if {[lindex $adefs 0] eq "Tcl_Interp*"} {
	set adefs [lrange $adefs 2 end]
    }

    set min 0 ; # count all non-optional argument. min required.
    set max 0 ; # count all arguments. max allowed.
    set names {}
    foreach {t a} $adefs {
	incr max
	if {[llength $a] == 1} {
	    incr min
	    lappend names $a
	} else {
	    lappend names ?[lindex $a 0]?
	}
    }

    incr min
    incr max
    incr min $offset
    incr max $offset

    set keep 1
    incr keep $offset

    set  names [join $names { }]
    if {$names eq {}} {
	set names NULL
    } else {
	set names \"$names\"
    }

    Emitln ""
    if {$min == $max} {
	Emitln "  if (oc != $min) \{"
    } else {
	Emitln "  if ((oc < $min) || ($max < oc)) \{"
    }
    Emitln "    Tcl_WrongNumArgs(interp, $keep, ov, $names);"
    Emitln "    return TCL_ERROR;"
    Emitln "  \}"
    Emitln ""
    return
}

proc ::critcl::EmitArgumentConversion {adefs offset} {
    incr offset
    foreach c [argconversion $adefs $offset] {
	Emitln $c
    }
    return
}

proc ::critcl::EmitCall {cname cnames rtype} {
    # Invoke the low-level function.

    Emitln  "  /* Call - - -- --- ----- -------- */"
    Emit "  "
    if {$rtype ne "void"} { Emit "rv = " }
    Emitln "${cname}([join $cnames {, }]);"
    Emitln
    return
}

proc ::critcl::EmitShimFooter {rtype} {
    # Convert the returned low-level result from C to Tcl, if required.
    # Return a standard status, if required.

    set code [typeconv::result-get-code $type $rtype]
    if {$code ne {}} { Emitln $code }
    Emitln \}
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - Management of in-memory C source fragment.

proc ::critcl::name2c {name} {
    # Note: A slightly modified copy (different depth in the
    # call-stack) of this code is inlined into the internal command
    # "BeginCommand".

    # Locate caller, as the data (embedded serial number) is computed
    # per .tcl file.
    set file [who::is]
    cdefs::initialize $file

    if {![string match ::* $name]} {
	# Locate caller's namespace. Two up, skipping the
	# ccommand/cproc frame. This is where the new Tcl command will
	# be defined in.

	set ns [uplevel 1 namespace current]
	if {$ns ne "::"} { append ns :: }

	set name ${ns}$name
    }

    # First ensure that any namespace qualifiers found in the name
    # itself are shifted over to the namespace information.

    set ns   [namespace qualifiers $name]
    set name [namespace tail       $name]

    # Then ensure that everything is fully qualified, and that the C
    # level name doesn't contain bad characters. We have to remove any
    # non-alphabetic characters. A serial number is further required
    # to distinguish identifiers which would, despite having different
    # Tcl names, transform to the same C identifier.

    if {$ns ne "::"} { append ns :: }
    set cns [string map {:: _} $ns]

    regsub -all -- {[^a-zA-Z0-9_]} $name _ cname
    regsub -all -- {_+} $cname _ cname

    regsub -all -- {[^a-zA-Z0-9_]} $cns _ cns
    regsub -all -- {_+} $cns _ cns

    set cname $cname[uuid::serial $file]

    return [list $ns $cns $name $cname]
}

proc ::critcl::BeginCommand {visibility name args} {
    # Locate caller, as the data is saved per .tcl file.
    # XXX FIXME get file reference as argument.
    set file [who::is]

    # Inlined name2c
    if {![string match ::* $name]} {
	# Locate caller's namespace. Two up, skipping the
	# ccommand/cproc frame. This is where the new Tcl command will
	# be defined in.

	set ns [uplevel 2 namespace current]
	if {$ns ne "::"} { append ns :: }

	set name ${ns}$name
    }

    # First ensure that any namespace qualifiers found in the name
    # itself are shifted over to the namespace information.

    set ns   [namespace qualifiers $name]
    set name [namespace tail       $name]

    # Then ensure that everything is fully qualified, and that the C
    # level identifiers don't contain bad characters. We have to
    # remove any non-alphabetic characters. A serial number is further
    # required to distinguish identifiers which would, despite having
    # different Tcl names, transform to the same C identifier.

    if {$ns ne "::"} { append ns :: }
    set cns [string map {:: _} $ns]

    regsub -all -- {[^a-zA-Z0-9_]} $name _ cname
    regsub -all -- {_+} $cname _ cname

    regsub -all -- {[^a-zA-Z0-9_]} $cns _ cns
    regsub -all -- {_+} $cns _ cns

    set cname $cname[uuid::serial $file]

    # Set the defered build-on-demand used by mode 'comile & run' up.
    # Note: Removing the leading :: because it trips Tcl's unknown
    # command, i.e. the command will not be found when called in a
    # script without leading ::.
    set ::auto_index([string trimleft $ns$name :]) [list [namespace current]::cbuild-auto $file]

    set v::curr [cdefs::func-begin $file $ns$name $cns$cname $args]

    if {$visibility eq "public"} {
	Emitln "#define ns_$cns$cname \"$ns$name\""
    }
    return [list $ns $cns $name $cname]
}

proc ::critcl::EndCommand {} {
    # XXX CHECK - what is this for?
    set v::code($v::curr) $v::block

    # XXX FIXME get file reference as argument.
    set file [who::is]
    cdefs::func-done $file $v::curr $v::block

    unset v::curr
    unset v::block
    return
}

proc ::critcl::Emit {s} {
    append v::block $s
    return
}

proc ::critcl::Emitln {{s ""}} {
    Emit $s\n
    return
}

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl {
    # keep all variables in a sub-namespace for easy access
    namespace eval v {
	# ----------------------------------------------------------------
	variable prefix ;# String. The string to start all file names
			 # generated by the package with. The prefix
	                 # is based on the package's version. This
	                 # allows multiple versions of the package to
	                 # use the same cache without interfering with
	                 # each. Note that we cannot use 'pid' and
	                 # similar information, because this would
	                 # circumvent the goal of the cache, the reuse
	                 # of binaries whose sources did not change.
	set prefix "v[package require critcl]"
	regsub -all {\.} $prefix {} prefix

	variable code	         ;# This array collects all code snippets and
				  # data about them.

	# Keys for 'code' (above) and their contents:
	#
	# <file> -> Per-file information, nested dictionary. Sub keys:
	#	config		- Collected code and configuration (ccode, etc.).
	#		license		- String. License text.
	#		meta		- Dictionary. Arbitrary keys to values, the user meta-data for the package.
	#		package		- Dictionary. Keys, see below. System meta data for the package. Values are lists.
	#			name		- Name of current package
	#			version		- Version of same.
	#			description	- Long description.
	#			summary		- Short description (one line).
	#			subject		- Keywords and -phrases.
	#			as::build::date	- Date-stamp for the build.
	#
	# ---------------------------------------------------------------------
	# NOTE: <file> are normalized absolute path names for exact
	#       identification of the relevant .tcl file.

	# _____________________________________________________________________
	# State used by "BeginCommand", "EndCommand", "Emit*" _________________

	variable curr	         ;# Hash of the last BeginCommand.
	variable block           ;# C code assembled by Emit* calls
				  # between Begin- and EndCommand.
    }
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl {
    namespace export \
	at cache ccode ccommand cdata cdefines cflags cheaders \
	check cinit clibraries compiled compiling config cproc \
	csources debug done failed framework ldflags platform \
	tk tsources preload license load tcl api userconfig meta \
	source include make
    # This is exported for critcl::app to pick up when generating the
    # dummy commands in the runtime support of a generated package.
    namespace export Ignore
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
