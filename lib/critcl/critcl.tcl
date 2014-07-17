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
package require critcl::api       ;# Management of stubs tables.
package require critcl::cache     ;# Result cache access.
package require critcl::ccconfig  ;# CC configuration database for standard backend.
package require critcl::common    ;# General utility commands.
package require critcl::data      ;# Access to templates and other supporting files.
package require critcl::log       ;# Log files within the result cache.
package require critcl::gopt      ;# Management of global options.
package require critcl::meta      ;# Management of teapot meta data.
package require critcl::scan      ;# Static Tcl code scanner.
package require critcl::tags      ;# Management of indicator flags.
package require critcl::typeconv  ;# Handling cproc data types.
package require critcl::usrconfig ;# Management of user options.
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

proc ::critcl::TeapotPlatform {} {
    # Platform identifier HACK. Most of the data in critcl is based on
    # 'platform::generic'. The TEApot MD however uses
    # 'platform::identify' with its detail information (solaris kernel
    # version, linux glibc version). But, if a cross-compile is
    # running we are SOL, because we have no place to pull the
    # necessary detail from, 'identify' is a purely local operation :(

    set platform [actualtarget]
    if {[platform::generic] eq $platform} {
	set platform [platform::identify]
    }

    return $platform
}

proc ::critcl::TeapotRequire {dspec} {
    # Syntax of dspec: (a) pname
    #             ...: (b) pname req-version...
    #             ...: (c) pname -exact req-version
    #
    # We can assume that the syntax is generally ok, because otherwise
    # the 'package require' itself will fail in a moment, blocking the
    # further execution of the .critcl file. So we only have to
    # distinguish the cases.

    if {([llength $dspec] == 3) &&
	([lindex $dspec 1] eq "-exact")} {
	# (c)
	lassign $dspec pn _ pv
	set spec [list $pn ${pv}-$pv]
    } else {
	# (a, b)
	set spec $dspec
    }

    return $spec
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Embed C Code

proc ::critcl::ccode {text} {
    set file [CheckEntry]
    set digest [uuid::add $file .ccode $text]

    set block {}
    lassign [at::header $text] leadoffset text
    append block [at::cpragma $leadoffset -2 $file] $text \n

    dict update v::code($file) config c {
	dict lappend c fragments $digest
	dict set     c block     $digest $block
	dict lappend c defs      $digest
    }
    return
}

proc ::critcl::ccommand {name anames args} {
    set file [CheckEntry]

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

    # XXX clientdata/delproc, either note clashes, or keep information per-file.

    set v::clientdata($key) $clientdata
    set v::delproc($key) $delproc

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
    uplevel 1 [list critcl::ccommand $name {dummy ip objc objv} [at::caller!]$body]
    return $name
}

proc ::critcl::cdefines {defines {namespace "::"}} {
    set file [CheckEntry]
    set digest [uuid::add $file .cdefines [list $defines $namespace]]

    dict update v::code($file) config c {
	foreach def $defines {
	    dict set c const $def $namespace
	}
    }
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

    set digesta [uuid::add $file .cinit.f $text]
    set digestb [uuid::add $file .cinit.e $edecls]

    set initc {}
    set skip [at::lines $text]
    lassign [at::header $text] leadoffset text
    if {[gopt::get lines]} {
	append initc [at::cpragma $leadoffset -2 $file]
    }
    append initc $text \n

    set edec {}
    lassign [at::header $edecls] leadoffset edecls
    if {[gopt::get lines]} {
	incr leadoffset $skip
	append edec [at::cpragma $leadoffset -2 $file]
    }
    append edec $edecls \n

    dict update v::code($file) config c {
	dict append  c initc  $initc \n
	dict append  c edecls $edec  \n
    }
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
    # Prefix prevents collision of slot names and file paths.
    who::push critcl://$slot
    return
}

proc ::critcl::collect_end {} {
    # Stop last diversion, and return the collected information as
    # single string of C code.

    # Ensure that a diversion is actually open.
    if {![who::depth]} {
	return -code error "collect_end mismatch, no diversions active"
    }

    set slot [who::pop]
    set block {}

    foreach digest [dict get $v::code($slot) config fragments] {
	append block "[Separator]\n\n"
	append block [dict get $v::code($slot) config block $digest]\n
    }

    # Drop all the collected data. Note how anything other than the C
    # code fragments is lost, and how cbuild results are removed
    # also. These do not belong anyway.
    unset v::code($slot)

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

    foreach f [Expand $file $path] {
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
    CheckEntry
    return [SetParam cheaders $args]
}

proc ::critcl::csources {args} {
    CheckEntry
    return [SetParam csources $args 1 1]
}

proc ::critcl::clibraries {args} {
    CheckEntry
    return [SetParam clibraries $args]
}

proc ::critcl::cobjects {args} {
    CheckEntry
    return [SetParam cobjects $args]
}

proc ::critcl::tsources {args} {
    set file [CheckEntry]
    # This, 'license', 'meta?' and 'meta' are the only places where we
    # are not extending the UUID. Because the companion Tcl sources
    # (count, order, and content) have no bearing on the binary at
    # all.
    InitializeFile $file

    dict update v::code($file) config c {
	foreach f $args {
	    foreach e [Expand $file $f] {
		dict lappend c tsources $e
	        scan-dependencies $file $e
	    }
	}
    }
    return
}

proc ::critcl::cflags {args} {
    set file [CheckEntry]
    if {![llength $args]} return

    uuid::add $file .cflags $args
    dict update v::code($file) config c {
	foreach flag $args {
	    dict lappend c cflags $flag
	}
    }
    return
}

proc ::critcl::ldflags {args} {
    set file [CheckEntry]
    if {![llength $args]} return

    uuid::add $file .ldflags $args
    dict update v::code($file) config c {
	foreach flag $args {
	    # Drop any -Wl prefix which will be added back a moment
	    # later, otherwise it would be doubled, breaking the command.
	    regsub -all {^-Wl,} $flag {} flag
	    dict lappend c ldflags -Wl,$flag
	}
    }
    return
}

proc ::critcl::framework {args} {
    CheckEntry

    # Check if we are building for OSX and ignore the command if we
    # are not. Our usage of "actualtarget" means that we allow for a
    # cross-compilation environment to OS X as well.
    if {![string match "macosx*" [actualtarget]]} return

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

    uuid::add $file .mintcl $version
    dict set v::code($file) config mintcl $version

    # This is also a dependency to record in the meta data. A 'package
    # require' is not needed. This can be inside of the generated and
    # loaded C code.

    meta::require $file [list Tcl $version]
    return
}

proc ::critcl::tk {} {
    set file [CheckEntry]

    uuid::add $file .tk 1
    dict set v::code($file) config tk 1

    # This is also a dependency to record in the meta data. A 'package
    # require' is not needed. This can be inside of the generated and
    # loaded C code.

    meta::require $file Tk
    return
}

# Register a shared library for pre-loading - this will eventually be
# redundant when TIP #239 is widely available
proc ::critcl::preload {args} {
    set file [CheckEntry]
    if {![llength $args]} return

    uuid::add $file .preload $args
    dict update v::code($file) config c {
	foreach lib $args {
	    dict lappend c preload $lib
	}
    }
    return
}

proc ::critcl::license {who args} {
    set file [CheckEntry]

    # This, 'tsources', 'meta?', and 'meta' are the only places where
    # we are not extending the UUID. Because the license text has no
    # bearing on the binary at all.
    InitializeFile $file

    eval [linsert $args 0 meta::license $file $who]
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: meta data (teapot)

proc ::critcl::description {text} {
    set file [CheckEntry]
    InitializeFile $file

    meta::description $file $text
    return
}

proc ::critcl::summary {text} {
    set file [CheckEntry]
    InitializeFile $file

    meta::summary $file $text
    return
}

proc ::critcl::subject {args} {
    set file [CheckEntry]
    InitializeFile $file

    eval [linsert $args 0 meta::subject $file]
    return
}

proc ::critcl::meta {key args} {
    set file [CheckEntry]
    # This, 'meta?', 'license', and 'tsources' are the only places
    # where we are not extending the UUID. Because the meta data has
    # no bearing on the binary at all.
    InitializeFile $file

    eval [linsert $args 0 meta::general $file $key]
    return
}

proc ::critcl::meta? {key} {
    set file [CheckEntry]
    # This, 'meta', 'license', and 'tsources' are the only places
    # where we are not extending the UUID. Because the meta data has
    # no bearing on the binary at all.
    InitializeFile $file

    return [meta::get $file $key]
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: user configuration options.

proc ::critcl::userconfig {cmd args} {
    set file [CheckEntry]
    InitializeFile $file

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

    set ok [CompileDirect $file $label $code]
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

    set ok [CompileLinkDirect $file $label $code]
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
    return [HasCompiler]
}

proc ::critcl::done {} {
    return [tags::has [SkipIgnored [who::is] 1] done]
}

proc ::critcl::failed {} {
    SkipIgnored [who::is] 0
    if {$v::buildforpackage} { return 0 }
    return [cbuild [who::is] 0]
}

proc ::critcl::load {} {
    SkipIgnored [who::is] 1
    if {$v::buildforpackage} { return 1 }
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
    if {[tags::has $f ignore]} { return -code return $result }
    return $f
}

proc ::critcl::AbortWhenCalledAfterBuild {} {
    # Inlined [done]. Simplified, always called after SkipIgnored.
    if {![tags::has [who::is] done]} return

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
    error "[lindex [info level -1] 0]$cloc: Illegal attempt to define C code in [who::is] after it was built."
}

proc ::critcl::CheckEntry {{result {}}} {
    set file [SkipIgnored [who::is] $result]
    AbortWhenCalledAfterBuild
    return $file
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
    # XXX FIXME - Use tag to hold the information ?
    # XXX FIXME - Or use the general ccode container to come?

    # Replace 'all' everywhere, and squash duplicates, whether from
    # this, or user-specified.
    set args [string map {all {memory symbols}} $args]
    set args [lsort -unique $args]

    foreach arg $args {
	switch -- $arg {
	    memory  {
		foreach x [ccconfig::get debug_memory]  { cflags $x }
	    }
	    symbols {
		foreach x [ccconfig::get debug_symbols] { cflags $x }
		set option::debug_symbols 1
	    }
	    default {
		error "unknown critcl::debug option - $arg"
	    }
	}
    }
    return
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

proc ::critcl::buildforpackage {{buildforpackage 1}} {
    set v::buildforpackage $buildforpackage
    return
}

proc ::critcl::cbuild {file {load 1}} {
    # Fast path for known result.
    if {[tags::has $file failed] && !$load} {
	set v::buildforpackage 0
	return [tags::get $file failed]
    }

    if {$file eq ""} {
	set file [who::is]
    }

    # Determine the requested mode and reset for next call.
    set buildforpackage $v::buildforpackage
    set v::buildforpackage 0

    # Complete stubs handling for the file/package.
    # This feeds a number of last-minute C fragments into the system.

    lassign [api::complete $file] \
	xxcname xxdefines xxflags xxcode xxdecls xxinit

    foreach i $xxinit d $xxdecls {
	cinit $i $d
    }
    foreach import $xxcode {
	ccode $import ;# (**)
    }

    # Begin with result dict here ... Keeping or not is handled after build/load...

    ## before this line - frontend operation
    # # ## ### ##### ######## #############
    ## after this line - backend execution
    ## arguments from frontend
    ## - file
    ## - buildforpackage
    ## - load

    StatusReset

    # Determine if we should place stubs code into the generated file.
    set placestubs [expr {!$buildforpackage}]

    # NOTE: The 4 pieces of data just below has to be copied into the
    # result even if the build and link-steps are suppressed. Because
    # the load-step must have this information.

    set base     [BaseOf             $file]
    set shlib    [DetermineShlibName $base]
    set initname [DetermineInitName  $file [expr {$buildforpackage ? "ns" : ""}]]
    set tsources [GetParam $file tsources]
    set mintcl   [MinTclVersion $file]

    # The result dictionary is local information.
    #	initname   - String. Foo in Foo_Init().
    #	tsources   - List. The companion tcl sources for <file>.
    #	object	   - String. Name of the object file backing <file>.
    #	objects	   - List. All object files, main and companions.
    #	shlib	   - String. Name of the shared library backing <file>.
    #	base	   - String. Common prefix (file root) of 'object' and 'shlib'.
    #	clibraries - List. See config. Copy for global linkage.
    #	ldflags	   - List. See config. Copy for global linkage.
    #	mintcl	   - String. Minimum version of Tcl required by the package.
    #	preload	   - List. Names of all libraries to load before
    #	             the package library.
    #	license    - String. License text.
    #	<= "critcl::cresults"

    dict set result base       $base
    dict set result shlib      $shlib
    dict set result initname   $initname
    dict set result tsources   $tsources
    dict set result mintcl     $mintcl

    catch {
	dict set result pkgname [meta::gets $file name]
    }

    if {[gopt::get force] || ![file exists $shlib]} {
	log::begin $v::prefix $file

	set object [DetermineObjectName $base $file]
	dict set result object $object

	# XXX This may run cinit and cflags to provide stubs api
	# XXX header information. I.e. it influences the collected C.
	# XXX As such it should possibly move to the head, keeping it
	# XXX as part of the frontend. Except it already saves data
	# XXX backend is using for itself.
	##
	# XXX FIXME TODO API_setup disentangle these intermingled functions and
	# XXX FIXME TODO API_setup responsibilities.

	# XXX FIXME cname, etc. can be empty (no stubs).

	# XXX uses apiprefix  => CollectEmbedded... header.c
	# XXX      apidefines => Compile ... cc cmdline
	# XXX      apiheader  => app-critcl

	# XXX apiprefix header.c vs [ccode] (see (**)) ?!

	dict set result apidefines $xxdefines
	if {[llength $xxcode]} {
	    dict set     result apiprefix  \n[join $xxcode \n]\n
	    dict lappend result apiheader [cache::get $xxcname]
	}

	if {[dict exists $result apiprefix]} {
	    set api [dict get $result apiprefix]
	} else {
	    set api ""
	}

	# Generate the main C file
	CollectEmbeddedSources $file $api $base.c $object $initname $placestubs

	# Set the marker for critcl::done and its user, AbortWhenCalledAfterBuild, CheckEntry.
	tags::set $file done

	# Compile main file
        lappend objects [Compile result $file $file $base.c $object]

	# Compile the companion C sources as well, if there are any.
        foreach src [GetParam $file csources] {
	    lappend objects [Compile result $file $src $src [CompanionObject $src]]
	}

	# NOTE: The information below has to be copied into the result
	# even if the link-step is suppressed. Because the application
	# (mode 'generate package') must know all this to be able to
	# perform the final link.

	lappendlist objects [GetParam $file cobjects]

	set ldflags [GetParam $file ldflags]
	set preload [GetParam $file preload]

	dict set result clibraries [GetParam $file clibraries]
	dict set result ldflags    $ldflags
	dict set result objects    $objects
	dict set result tk         [UsingTk  $file]
	dict set result preload    $preload
	dict set result license    [GetParam $file license <<Undefined>>]
	dict set result log        {}
	dict set result meta       [meta::getall $file]

	# Link and load steps.
        if {$load || !$buildforpackage} {
	    Link result $file $shlib $preload $ldflags
	}

	set msgs [log::done]
	dict set result warnings [CheckForWarnings $msgs]
    }

    if {$v::failed} {
	if {!$buildforpackage} {
	    print stderr "$msgs\ncritcl build failed ($file)"
	} else {
	    dict set result log $msgs
	}
    } elseif {$load && !$buildforpackage} {
	Load $shlib $initname $tsources
    }

    # Store collected results for pickup through "cresults".
    tags::set $file result $result

    # Save final status
    tags::set $file failed $v::failed
    StatusReset

    ## before this line - backend execution
    # # ## ### ##### ######## #############
    ## after this line - frontend operation

    # Release the data which was collected for the just-built file, as
    # it is not needed any longer.
    dict unset v::code($file) config
    uuid::clear      $file
    usrconfig::clear $file
    api::clear       $file
    # replacement for v::code($file) FIXME TODO

    return [tags::get $file failed]
}

proc ::critcl::cresults {{file {}}} {
    if {$file eq ""} { set file [who::is] }
    return [tags::get $file result]
}

proc ::critcl::cnothingtodo {f} {
    # No critcl definitions at all ?
    # We have results already, so where had been something to do.

    if {![tags::has $f def]} { return 1 }

    if {![info exists  v::code($f)]} { return 1 }
    if {[dict exists $v::code($f) result]} { return 0 }

    # No C code collected for compilation ?
    if {![dict exists $v::code($f) config fragments]} { return 1 }

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
## Implementation -- Internals - Manage complex per-file settings.

proc ::critcl::GetParam {file type {default {}}} {
    if {[info exists  v::code($file)] &&
	[dict exists $v::code($file) config $type]} {
	return [dict get $v::code($file) config $type]
    } else {
	return $default
    }
}

proc ::critcl::SetParam {type values {expand 1} {uuid 0}} {
    # XXX review call sites to note which combinations of expand and
    # XXX uuid are in actual use.
    # XXX Note: uuid is only effective under expand.

    set file [who::is]
    if {![llength $values]} return
    InitializeFile $file

    uuid::add $file .$type $values

    if {[llength $values]} {
	# Process the list of flags, treat non-option arguments as
	# glob patterns and expand them to a set of files, stored as
	# absolute paths.
	set tmp {}
	foreach v $values {
	    if {[string match "-*" $v]} {
		lappend tmp $v
	    } else {
		if {$expand} {
		    if {$uuid} {
			foreach f [Expand $file $v] {
			    lappend tmp $f
			    uuid::add $file .$type.$f [common::cat $f]
			}
		    } else {
			lappendlist tmp [Expand $file $v]
		    }
		} else {
		    lappend tmp $v
		}
	    }
	}

	# And save into the system state.
	dict update v::code($file) config c {
	    foreach v $tmp {
		dict lappend c $type $v
	    }
	}
    } elseif {[dict exists $v::code($file) config $type]} {
	return [dict get $v::code($file) config $type]
    }
}

proc ::critcl::Expand {file pattern} {
    set base [file dirname $file]
    return [common::expand-glob $base $pattern]
}

proc ::critcl::InitializeFile {file} {
    # XXX FIXME TODO remove 'v::code($file)' entirely
    if {![info exists v::code($file)]} {
	set v::code($file) {}
    }

    # XXX FIXME TODO remove 'v::code($file) config' entirely
    if {![dict exists $v::code($file) config]} {
	dict set v::code($file) config {}
    }

    if {![tags::has $file def]} {
	tags::set $file def

	# Initialize the system meta data.
	# User meta data auto-initializes on write.

	meta::assign $file platform    [list [TeapotPlatform]]
	meta::assign $file build::date [list [common::today]]

	# May not exist, bracket code.
	if {![file exists $file]} return

	scan-dependencies $file $file provide
	return
    }
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - Management of in-memory C source fragment.

proc ::critcl::name2c {name} {
    # Note: A slightly modified copy (different depth in the call-stack) of this
    # is inlined into the internal command "BeginCommand".

    # Locate caller, as the data is saved per .tcl file.
    set file [who::is]

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
    set ::auto_index([string trimleft $ns$name :]) [list [namespace current]::cbuild $file]

    set v::curr [uuid::add $file .function "$ns $name $args"]

    dict update v::code($file) config c {
	dict lappend c functions $cns$cname
	dict lappend c fragments $v::curr
    }

    if {$visibility eq "public"} {
	Emitln "#define ns_$cns$cname \"$ns$name\""
    }
    return [list $ns $cns $name $cname]
}

proc ::critcl::EndCommand {} {
    set file [who::is]

    set v::code($v::curr) $v::block

    dict set v::code($file) config block $v::curr $v::block

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
## Backend, Checking that we have a compiler

proc ::critcl::HasCompiler {} {
    return [llength [auto_execok [lindex [ccconfig::get compile] 0]]]
}

# # ## ### ##### ######## ############# #####################
## Backend, Direct compiling, linking of test C sources.

proc ::critcl::CompileDirect {file label code {mode temp}} {
    set src [cache::write check_[pid].c $code]
    set obj [file rootname $src][ccconfig::get object]

    # See also the internal helper command 'Compile'. The code here is
    # in essence a simplified form of that.

    set         cmdline [ccconfig::get compile]
    lappendlist cmdline [GetParam $file cflags]
    lappendlist cmdline [SystemIncludes $file]
    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $src

    log::begin $v::prefix $file
    log::text  "${label}... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK FAILED]
    StatusReset

    if {!$ok || ($mode eq "temp")} {
	log::done
	cache::clear check_[pid].*
    }
    return $ok
}

proc ::critcl::CompileLinkDirect {file label code} {
    set ok [CompileDirect $file "$label (build)" $code keep]
    if {!$ok} {
	return 0
    }

    set out [cache::get check_[pid].out]
    set obj [file rootname $out][ccconfig::get object]

    set cmdline [ccconfig::get link]

    if {$option::debug_symbols} {
	lappendlist cmdline [ccconfig::get link_debug]
    } else {
	lappendlist cmdline [ccconfig::get strip]
	lappendlist cmdline [ccconfig::get link_release]
    }

    lappendlist cmdline [LinkResult $out]
    lappendlist cmdline $obj
    lappendlist cmdline [SystemLibraries]
    lappendlist cmdline [FixLibraries [GetParam $file clibraries]]
    lappendlist cmdline [GetParam $file ldflags]

    log::text "${label} (link)... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK ERR]

    log::done
    cache::clear check_[pid].*
    return $ok
}

# # ## ### ##### ######## ############# #####################
## Backend Processing, Collected Sources

proc ::critcl::CollectEmbeddedSources {file api destination libfile ininame placestubs} {
    # Start assembly.

    set fd [open $destination w]

    # Boilerplate header.
    puts $fd [subst [common::cat [data::cfile header.c]]]
    #         ^=> file, libfile, api

    # Make Tk available, if requested
    if {[UsingTk $file]} {
	puts $fd "\n#include \"tk.h\""
    }

    # Write the collected C fragments, in order of collection.
    foreach digest [GetParam $file fragments] {
	puts $fd "[Separator]\n"
	puts $fd [dict get $v::code($file) config block $digest]
    }

    # Boilerplate trailer.
    # Stubs setup, Tcl, and, if requested, Tk as well.
    puts $fd [Separator]
    set mintcl [MinTclVersion $file]

    if {$placestubs} {
	# Put full stubs definitions into the code, which can be
	# either the bracket generated for a -pkg, or the package
	# itself, build in mode "compile & run".
	set stubs     [data::tcl-decls      $mintcl]
	set platstubs [data::tcl-plat-decls $mintcl]
	puts -nonewline $fd [subst [common::cat [data::cfile stubs.c]]]
	#                    ^=> mintcl, stubs, platstubs
    } else {
	# Declarations only, for linking, in the sub-packages.
	puts -nonewline $fd [subst [common::cat [data::cfile stubs_e.c]]]
	#                    ^=> mintcl
    }

    if {[UsingTk $file]} {
	SetupTkStubs $fd
    }

    # Initialization boilerplate. This ends in the middle of the
    # FOO_Init() function, leaving it incomplete.

    set ext [GetParam $file edecls]
    puts $fd [subst [common::cat [data::cfile pkginit.c]]]
    #         ^=> ext, ininame

    # From here on we are completing FOO_Init().
    # Tk setup first, if requested. (Tcl is already done).
    if {[UsingTk $file]} {
	puts $fd [common::cat [data::cfile pkginittk.c]]
    }

    # User specified initialization code.
    puts $fd "[GetParam $file initc] "

    # Setup of the variables serving up defined constants.
    if {[dict exists $v::code($file) config const]} {
	BuildDefines $fd $file
    }

    # Take the names collected earlier and register them as Tcl
    # commands.
    foreach name [lsort [GetParam $file functions]] {
	if {[info exists v::clientdata($name)]} {
	    set cd $v::clientdata($name)
	} else {
	    set cd NULL
	}
	if {[info exists v::delproc($name)]} {
	    set dp $v::delproc($name)
	} else {
	    set dp 0
	}
	puts $fd "  Tcl_CreateObjCommand(ip, ns_$name, tcl_$name, $cd, $dp);"
    }

    # Complete the trailer and be done.
    puts  $fd [common::cat [data::cfile pkginitend.c]]
    close $fd
    return
}

proc ::critcl::MinTclVersion {file} {
    set required [GetParam $file mintcl 8.4]
    foreach version [data::available-tcl] {
	if {[package vsatisfies $version $required]} {
	    return $version
	}
    }
    return $required
}

proc ::critcl::UsingTk {file} {
    return [GetParam $file tk 0]
}

proc ::critcl::TclIncludes {tclversion} {
    # Provide access to the Tcl/Tk headers using a -I flag pointing
    # into the critcl package directory hierarchy. No copying of files
    # required. This also handles the case of the X11 headers on
    # windows, for free.

    set path [data::hdr tcl$tclversion]
    if {[file system $path] ne "native"} {
	# The critcl package is wrapped. Copy the relevant headers out
	# to disk (cache) and change the include path appropriately.
	set path [cache::copy2 $path]
    }

    return [list [ccconfig::get include]$path]
}

proc ::critcl::SystemIncludes {file} {
    set includes {}
    foreach dir [SystemIncludePaths $file] {
	lappend includes [ccconfig::get include]$dir
    }
    return $includes
}

proc ::critcl::SystemIncludePaths {file} {
    set paths {}
    set has {}

    # critcl -I options.
    foreach dir [gopt::get I] {
	if {[dict exists $has $dir]} continue
	dict set has $dir yes
	lappend paths $dir
    }

    # Result cache may be source of header files too.
    lappend paths [cache::get]

    # critcl::cheaders
    foreach flag [GetParam $file cheaders] {
	if {![string match "-*" $flag]} {
	    # flag = normalized absolute path to a header file.
	    # Transform into a directory reference.
	    set dir [file dirname $flag]
	} else {
	    # Chop leading -I
	    set dir [string range $flag 2 end]
	}

	if {[dict exists $has $dir]} continue
	dict set has $dir yes
	lappend paths $dir
    }

    return $paths
}

proc ::critcl::SystemLibraries {} {
    set libincludes {}
    foreach dir [SystemLibraryPaths] {
	lappend libincludes [ccconfig::get libinclude]$dir
    }
    return $libincludes
}

proc ::critcl::SystemLibraryPaths {} {
    set paths {}
    set has {}

    # critcl -L options.
    foreach dir [gopt::get L] {
	if {[dict exists $has $dir]} continue
	dict set has $dir yes
	lappend paths $dir
    }

    return $paths
}

proc ::critcl::Compile {rv tclfile origin cfile obj} {
    upvar 1 $rv result

    StatusAbort?

    # tclfile = The .tcl file under whose auspices the C is compiled.
    # origin  = The origin of the C sources, either tclfile, or cfile.
    # cfile   = The file holding the C sources to compile.
    #
    # 'origin == cfile' for the companion C files of a critcl file,
    # i.e. the csources. For a .tcl critcl file, the 'origin ==
    # tclfile', and the cfile is the .c derived from tclfile.
    #
    # obj = Object file to compile to, to generate.

    set         cmdline [ccconfig::get compile]
    lappendlist cmdline [GetParam $tclfile cflags]
    lappendlist cmdline [ccconfig::get threadflags]
    if {[gopt::get combine] ne "standalone"} {
	lappendlist cmdline [ccconfig::get tclstubs]
    }
    if {[gopt::get language] ne "" && [file tail $tclfile] ne "critcl.tcl"} {
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	# See also -x none below.
	lappend cmdline -x [gopt::get language]
    }
    lappendlist cmdline [TclIncludes [MinTclVersion $tclfile]]
    lappendlist cmdline [SystemIncludes $tclfile]

    if {[dict exists $result apidefines]} {
	lappendlist cmdline [dict get $result apidefines]
    }

    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $cfile

    if {[gopt::get language] ne ""} {
	# Allow the compiler to determine the type of file otherwise
	# it will try to compile the libs
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	lappend cmdline -x none
    }

    # Add the Tk stubs to the command line, if requested and not suppressed
    if {[UsingTk $tclfile] && ([gopt::get combine] ne "standalone")} {
	lappendlist cmdline [ccconfig::get tkstubs]
    }

    if {!$option::debug_symbols} {
	lappendlist cmdline [ccconfig::get optimize]
	lappendlist cmdline [ccconfig::get noassert]
    }

    if {[ExecWithLogging $cmdline \
	     {$obj: [file size $obj] bytes} \
	     {ERROR while compiling code in $origin:}]} {
	if {![gopt::get keepsrc] && $cfile ne $origin} {
	    file delete $cfile
	}
    }

    return $obj
}

proc ::critcl::MakePreloadLibrary {rv file} {
    upvar 1 $rv result

    StatusAbort?

    # compile and link the preload support, if necessary, i.e. not yet
    # done.

    set shlib [cache::get preload[ccconfig::get sharedlibext]]
    if {[file exists $shlib]} return

    # Operate like TclIncludes. Use the template file directly, if
    # possible, or, if we reside in a virtual filesystem, copy it to
    # disk.

    set src [data::cfile preload.c]
    if {[file system $src] ne "native"} {
	set src [cache::copy2 $src]
    }

    # Build the object for the helper package, 'preload' ...

    set obj [cache::get preload.o]
    Compile result $file $src $src $obj

    # ... and link it.
    # Custom linker command. XXX Can we bent Link to the task?
    set         cmdline [ccconfig::get link]
    lappend     cmdline $obj
    lappendlist cmdline [ccconfig::get strip]
    lappendlist cmdline [LinkResult $shlib]

    ExecWithLogging $cmdline \
	{$shlib: [file size $shlib] bytes} \
	{ERROR while linking $shlib:}

    # Now the critcl application can pick up this helper shlib and
    # stuff it into the package it is making.
    return
}

proc ::critcl::Link {rv file shlib preload ldflags} {
    upvar 1 $rv result

    StatusAbort?

    # Assemble the link command.
    set cmdline [ccconfig::get link]

    if {[llength $preload]} {
	lappendlist cmdline [ccconfig::get link_preload]
    }

    if {$option::debug_symbols} {
	lappendlist cmdline [ccconfig::get link_debug]
    } else {
	lappendlist cmdline [ccconfig::get strip]
	lappendlist cmdline [ccconfig::get link_release]
    }

    lappendlist cmdline [LinkResult $shlib]
    lappendlist cmdline [GetObjects $result]
    lappendlist cmdline [SystemLibraries]
    lappendlist cmdline [GetLibraries $result]
    lappendlist cmdline $ldflags
    # lappend cmdline bufferoverflowU.lib ;# msvc >=1400 && <1500 for amd64

    # Run the linker
    ExecWithLogging $cmdline \
	{$shlib: [file size $shlib] bytes} \
	{ERROR while linking $shlib:}

    # Now, if there is a manifest file around, and the
    # 'embed_manifest' command defined we use its command to merge the
    # manifest into the shared library. This is pretty much only
    # happening on Windows platforms, and with newer dev environments
    # actually using manifests.

    set em [ccconfig::get embed_manifest]

    log::line "Manifest Command: $em"
    log::line "Manifest File:    [expr {[file exists $shlib.manifest]
	   ? "$shlib.manifest"
	   : "<<not present>>, ignored"}]"

    if {[llength $em] && [file exists $shlib.manifest]} {
	set cmdline [ManifestCommand $em $shlib]

	# Run the manifest tool
	ExecWithLogging $cmdline \
	    {$shlib: [file size $shlib] bytes, with manifest} \
	    {ERROR while embedding the manifest into $shlib:}
    }

    # At last, build the preload support library, if necessary.
    if {[llength $preload]} {
	MakePreloadLibrary result $file
    }
    return
}

proc ::critcl::ManifestCommand {em shlib} {
    # Variable used by the subst'able config setting.
    set outfile $shlib
    return [subst $em]
}

proc ::critcl::CompanionObject {src} {
    set tail    [file tail $src]
    set srcbase [file rootname $tail]
    if {[file dirname $src] ne [cache::get]} {
	# The .c file does not reside in the cache directory.  Change
	# the source base so that the generated object file, which
	# will be put into the cache, does not collide with the object
	# files of other .c files with the same name.  This is done by
	# adding the last segment of the directory the file is in to
	# the base. While this can still lead to collisions the
	# probability should be low(er).
	set srcbase [file tail [file dirname $src]]_$srcbase
    }

    return [cache::get ${srcbase}[ccconfig::get object]]

    # Examples, with a .c file found in- and out-side of the cache.
    ##
    # (1) src     = $cache/foo.c
    #     tail    = foo.c
    #     srcbase = foo
    #     object  = $cache/foo.o
    ##
    # (2) src      = /some/other/foo.c
    #     tail     = foo.c
    #     srcbase  = foo
    #     srcbase' = other_foo
    #     object   = $cache/other_foo.o
}

proc ::critcl::CompileResult {object} {
    # Variable used by the subst'able config setting.
    set outfile $object
    return [subst [ccconfig::get output]]
}

proc ::critcl::LinkResult {shlib} {
    # Variable used by the subst'able config setting.
    set outfile $shlib

    set ldout [subst [ccconfig::get ldoutput]]
    if {$ldout eq ""} {
	set ldout [subst [ccconfig::get output]]
    }

    return $ldout
}

proc ::critcl::GetObjects {result} {
    # On windows using the native MSVC compiler put the companion
    # object files into a link file to read, instead of separately on
    # the command line.

    set objects [dict get $result objects]

    if {![string match "win32-*-cl" [ccconfig::buildplatform]]} {
	return $objects
    }

    set rsp [cache::write link.fil \"[join $objects \"\n\"]\"]
    return [list @$rsp]
}

proc ::critcl::GetLibraries {result} {
    # On windows using the native MSVC compiler, transform all -lFOO
    # references into FOO.lib.
    return [FixLibraries [dict get $result clibraries]]
}

proc ::critcl::FixLibraries {libraries} {
    if {[string match "win32-*-cl" [ccconfig::buildplatform]]} {
	# On windows using the native MSVC compiler, transform all
	# -lFOO references into FOO.lib.

	regsub -all -- {-l(\S+)} $libraries {\1.lib} libraries
    } else {
	# On unix we look for '-l:' references and rewrite them to the
	# full path of the library, doing the search on our own.
	#
	# GNU ld understands this since at least 2.22 (don't know if
	# earlier, 2.15 definitely doesn't), and it helps in
	# specifying static libraries (Regular -l prefers .so over .a,
	# and -l: overrides that).

	# Search paths specified via -L, -libdir.
	set lpath [SystemLibraryPaths]

	set tmp {}
	foreach word $libraries {
	    # Extend search path with -L options from clibraries.
	    if {[string match -L* $word]} {
		lappend lpath [string range $word 2 end]
		lappend tmp $word
		continue
	    }
	    if {![string match -l:* $word]} {
		lappend tmp $word
		continue
	    }
	    # Search named library.
	    lappend tmp [ResolveColonSpec $lpath [string range $word 3 end]]
	}
	set libraries $tmp
    }

    return $libraries
}

proc ::critcl::ResolveColonSpec {lpath name} {
    foreach path $lpath {
	set f [file join $lpath $name]
	if {![file exists $f]} continue
	return $f
    }
    return -l:$name
}

proc ::critcl::SetupTkStubs {fd} {
    puts -nonewline $fd [common::cat [data::cfile tkstubs.c]]
    return
}

proc ::critcl::BuildDefines {fd file} {
    # we process the cdefines in three steps
    #   - get the list of defines by preprocessing the source using the
    #     cpp -dM directive which causes any #defines to be output
    #   - extract the list of enums using regular expressions (not perfect,
    #     but will do for now)
    #   - generate Tcl_ObjSetVar2 commands to initialise Tcl variables

    # Pull the collected ccode blocks together into a transient file
    # we then search in.

    set def   define_[pid].c
    set dcode {}
    foreach digest [dict get $v::code($file) config defs] {
	append dcode [dict get $v::code($file) config block $digest]
    }
    set defpath [cache::write $def $dcode]

    # For the command lines to be constructed we need all the include
    # information the regular files will get during their compilation.

    set hdrs [SystemIncludes $file]

    # The result of the next two steps, a list of triples (namespace +
    # label + value) of the defines to export.

    set defines {}

    # First step - get list of matching defines
    set         cmd [ccconfig::get preproc_define]
    lappendlist cmd $hdrs
    lappend     cmd $defpath

    set pipe [open "| $cmd" r]
    while {[gets $pipe line] >= 0} {
	# Check if the line contains a define.
	set fields [split [string trim $line]]
	if {[lindex $fields 0] ne "#define"} continue

	# Yes. Get name and value. The latter is the joining of all
	# fields after the name, except for any enclosing parentheses,
	# which we strip off.

	set var [lindex $fields 1]
	set val [string trim [join [lrange $fields 2 end]] {()}]

	# We ignore however any and all defines the user is not
	# interested in making public. This is, in essence, a set
	# intersection on the names of the defines.

	if {![TakeDefine $file $var namespace]} continue

	# And for those which are kept we integrate the information
	# from both sources, i.e. namespace, and definition, under a
	# single name.

	lappend defines $namespace $var $val
    }
    close $pipe

    # Second step - get list of enums

    set         cmd [ccconfig::get preproc_enum]
    lappendlist cmd $hdrs
    lappend     cmd $defpath

    set pipe [open "| $cmd" r]
    set code [read $pipe]
    close $pipe

    set matches [regexp -all -inline {enum [^\{\(\)]*{([^\}]*)}} $code]
    foreach {match submatch} $matches {
	foreach line [split $submatch \n] {
	    foreach sub [split $line ,] {
		set enum [lindex [split [string trim $sub]] 0]

		# We ignore however any and all enum values the user
		# is not interested in making public. This is, in
		# essence, a set intersection on the names of the
		# enum values.

		if {![TakeDefine $file $enum namespace]} continue

		# And for those which are kept we integrate the
		# information from both sources, i.e. namespace, and
		# definition, under a single name.

		lappend defines $namespace $enum $enum
	    }
	}
    }

    # Third step - generate Tcl_ObjSetVar2 commands exporting the
    # defines and their values as Tcl variables.

    foreach {namespace constname constvalue} $defines {
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

    # Cleanup after ourselves, removing the helper file.
    if {![gopt::get keepsrc]} {
	cache::clear $def
    }
    return
}

proc ::critcl::TakeDefine {file identifier nsvar} {
    upvar 1 $nsvar dst
    if 0 {if {[dict exists $v::code($file) config const $identifier]} {
	set dst [dict get $v::code($file) config const $identifier]
	return 1
    }}
    foreach {pattern def} [dict get $v::code($file) config const] {
	if {[string match $pattern $identifier]} {
	    set dst $def
	    return 1
	}
    }
    return 0
}

proc ::critcl::Load {shlib init tsrc} {
    # Using the renamed builtin. While this is a dependency it was
    # recorded already. See 'critcl::tcl', and 'critcl::tk'.
    #package require Tcl $minv
    ::load $shlib $init

    # With the binary part loaded it is now time to load the Tcl
    # companion files. Note the use of "Ignore" to prevent issues if a
    # .critcl file specified itself as a Tcl companion, i.e. disabling
    # the processing of critcl directives. The "critcl" application
    # will place equivalent code into the "ifneeded" script of the
    # packages it generates.

    foreach t $tsrc {
	Ignore $t
	source $t
    }
    return
}

proc ::critcl::DetermineShlibName {base} {
    # The name of the shared library we hope to produce (or use)
    return ${base}[ccconfig::get sharedlibext]
}

proc ::critcl::DetermineObjectName {base file} {
    set object $base

    # The generated object file will be saved for permanent use if the
    # outdir option is set (in which case rebuilds will no longer be
    # automatic).

    set odir [gopt::get outdir]
    if {$odir ne ""} {
	set odir [file join [file dirname $file] $odir]
	set oroot  [file rootname [file tail $file]]
	set object [file normalize [file join $odir $oroot]]
	file mkdir $odir
    }

    # Modify the output file name if debugging symbols are requested.
    if {$option::debug_symbols} {
        append object _g
    }

    # Choose a distinct suffix so switching between them causes a
    # rebuild.
    switch -- [gopt::get combine] {
	""         -
	dynamic    { append object _pic  [ccconfig::get object] }
	static     { append object _stub [ccconfig::get object] }
	standalone { append object       [ccconfig::get object] }
    }

    return $object
}

proc ::critcl::DetermineInitName {file prefix} {
    set ininame [PkgInit $file]

    # Add in the build prefix, if specified. This is done in mode
    # 'generate package', for the pieces, ensuring that the overall
    # initialization function cannot be in conflict with the
    # initialization functions of these same pieces.

    if {$prefix ne ""} {
        set ininame "${prefix}_$ininame"
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

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - UUID management, change detection

proc ::critcl::BaseOf {f} {
    # Basename for all generated files (.c, .o, .so)
    return [file normalize [cache::get ${v::prefix}_[uuid::get $f]]]
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - Miscellanea

proc ::critcl::Separator {} {
    return "/* [string repeat - 70] */"
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - Status Operations, and execution
## of external commands.

proc ::critcl::StatusReset {} {
    set v::failed 0
    return
}

proc ::critcl::StatusAbort? {} {
    if {$v::failed} { return -code return }
    return
}

proc ::critcl::CheckForWarnings {text} {
    set warnings [dict create]
    foreach line [split $text \n] {
	# Ignore everything not a warning.
        if {![string match -nocase *warning* $line]} continue
	# Ignore duplicates (which is why we store the lines as dict
	# keys for now).
	if {[dict exists $warnings $line]} continue
	dict set warnings $line .
    }
    return [dict keys $warnings]
}

proc ::critcl::ExecWithLogging {cmdline okmsg errmsg} {
    set w [join [lassign $cmdline cmd] \n\t]
    log::text \n$cmd\n\t$w\n

    set ok [ccconfig::do-log v::failed v::err [log::fd] $cmdline]

    if {$ok} {
	log::line [uplevel 1 [list subst $okmsg]]
    } else {
	log::line [uplevel 1 [list subst $errmsg]]
	log::line $v::err
    }

    return $ok
}

# # ## ### ##### ######## ############# #####################
## Initialization

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl {
    # namespace to flag when options set
    namespace eval option {
        variable debug_symbols  0
    }

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

	# XXX clientdata() per-command (See ccommand). per-file+ccommand better?
	# XXX delproc()    per-command (See ccommand). s.a

	variable code	         ;# This array collects all code snippets and
				  # data about them.

	# Keys for 'code' (above) and their contents:
	#
	# <file> -> Per-file information, nested dictionary. Sub keys:
	#	config		- Collected code and configuration (ccode, etc.).
	#		tsources	- List. The companion tcl sources for <file>.
	#				  => "critcl::tsources".
	#		cheaders	- List. => "critcl::cheaders"
	#		csources	- List. => "critcl::csources"
	#		clibraries	- List. => "critcl::clibraries"
	#		cflags		- List. => "critcl::cflags", "critcl::framework",
	#					   "critcl::debug", "critcl::include"
	#		ldflags		- List. => "critcl::ldflags", "critcl::framework"
	#		initc		- String. Initialization code for Foo_Init(), "critcl::cinit"
	#		edecls		- String. Declarations of externals needed by Foo_Init(), "critcl::cinit"
	#		functions	- List. Collected function names.
	#		fragments	- List. Hashes of the collected C source bodies (functions, and unnamed code).
	#		block		- Dictionary. Maps the hashes to their C sources for fragments.
	#		defs		- List. Hashes of the collected C source bodies (only unnamed code), for extraction of defines.
	#		const		- Dictionary. Maps the names of defines to the namespace their variables will be in.
	#		mintcl		- String. Minimum version of Tcl required by the package.
	#		preload		- List. Names of all libraries to load
	#				  before the package library. This
	#				  information is used only by mode
	#				  'generate package'. This means that
	#				  packages with preload can't be used
	#				  in mode 'compile & run'.
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
	#
	# 'ccode'     -> Accumulated in-memory storage of code-fragments.
	#                Extended by 'ccode', used by 'BuildDefines',
	#                called by 'cbuild'. Apparently tries to extract defines
	#                and enums, and their values, for comparison with 'cdefine'd
	#		 values.
	#
	# NOTE: <file> are normalized absolute path names for exact
	#       identification of the relevant .tcl file.

	# _____________________________________________________________________
	# State used by "cbuild" ______________________________________________

	variable failed  0       ;# Build status. Used by "Status*"
	variable err     ""	 ;# and "Exec*". Build error text.

	variable buildforpackage 0 ;# Boolean flag controlling
				    # cbuild's behaviour. Named after
				    # the mode 'generate package'.
				    # Auto-resets to OFF after each
				    # call of "cbuild". Can be activated
				    # by "buildforpackage".

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
