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

if {[package vsatisfies [package present Tcl] 8.5]} {
    # 8.5+
    proc ::critcl::lappendlist {lvar list} {
	if {![llength $list]} return
	upvar $lvar dest
	lappend dest {*}$list
	return
    }
} else {
    # 8.4
    proc ::critcl::lappendlist {lvar list} {
	if {![llength $list]} return
	upvar $lvar dest
	set dest [eval [linsert $list 0 linsert $dest end]]
	#set dest [concat $dest $list]
	return
    }
}

# # ## ### ##### ######## ############# #####################

package require critcl::common   ;# General utility commands.
package require critcl::uuid     ;# UUID generation.
package require critcl::typeconv ;# Handling cproc data types.
package require critcl::who      ;# Management of current file.
package require critcl::scan     ;# Static Tcl code scanner.
package require critcl::at       ;# Management of #line pragmas.
# API exported through critcl core
# ::critcl::at::
#   caller  - stash caller location, possibly modified (level change, line offset)
#   caller! - format & return caller location, clears stash
#   here    - stash current location
#   here!   - return format & return  current location, clears stash
#   incr*   - modify stashed location (only line number, not file).
#   get     - format, return, and clear stash
#   get*    - format & return stash

# Define a few shims for public critcl APIs which are now served by
# the utility packages.

interp alias {} ::critcl::clean_cache    {} ::critcl::cache::clear
interp alias {} ::critcl::argtype        {} ::critcl::typeconv::arg-def
interp alias {} ::critcl::argtypesupport {} ::critcl::typeconv::arg-set-support
interp alias {} ::critcl::resulttype     {} ::critcl::typeconv::result-def

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
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
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
    SkipIgnored [set file [who::is]]
    AbortWhenCalledAfterBuild

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
	if {[at::enabled]} {
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
    SkipIgnored [who::is]
    AbortWhenCalledAfterBuild
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

    set body [subst [common::cat [Template cdata.c]]]
    #               ^=> count, inittext

    # NOTE: The uplevel is needed because otherwise 'ccommand' will
    # not properly determine the caller's namespace.
    uplevel 1 [list critcl::ccommand $name {dummy ip objc objv} [at::caller!]$body]
    return $name
}

proc ::critcl::cdefines {defines {namespace "::"}} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
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
    SkipIgnored [set file [who::is]]
    AbortWhenCalledAfterBuild

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
	if {[at::enabled]} {
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
    set file [SkipIgnored [set file [who::is]]]
    AbortWhenCalledAfterBuild

    set digesta [uuid::add $file .cinit.f $text]
    set digestb [uuid::add $file .cinit.e $edecls]

    set initc {}
    set skip [at::lines $text]
    lassign [at::header $text] leadoffset text
    if {[at::enabled]} {
	append initc [at::cpragma $leadoffset -2 $file]
    }
    append initc $text \n

    set edec {}
    lassign [at::header $edecls] leadoffset edecls
    if {[at::enabled]} {
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
    SkipIgnored [set file [who::is]]
    AbortWhenCalledAfterBuild

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
    SkipIgnored [who::is]
    AbortWhenCalledAfterBuild
    return [SetParam cheaders $args]
}

proc ::critcl::csources {args} {
    SkipIgnored [who::is]
    AbortWhenCalledAfterBuild
    return [SetParam csources $args 1 1]
}

proc ::critcl::clibraries {args} {
    SkipIgnored [who::is]
    AbortWhenCalledAfterBuild
    return [SetParam clibraries $args]
}

proc ::critcl::cobjects {args} {
    SkipIgnored [who::is]
    AbortWhenCalledAfterBuild
    return [SetParam cobjects $args]
}

proc ::critcl::tsources {args} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
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
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
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
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
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
    SkipIgnored [who::is]
    AbortWhenCalledAfterBuild

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
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild

    uuid::add $file .mintcl $version
    dict set v::code($file) config mintcl $version

    # This is also a dependency to record in the meta data. A 'package
    # require' is not needed. This can be inside of the generated and
    # loaded C code.

    ImetaAdd $file require [list [list Tcl $version]]
    return
}

proc ::critcl::tk {} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild

    uuid::add $file .tk 1
    dict set v::code($file) config tk 1

    # This is also a dependency to record in the meta data. A 'package
    # require' is not needed. This can be inside of the generated and
    # loaded C code.

    ImetaAdd $file require Tk
    return
}

# Register a shared library for pre-loading - this will eventually be
# redundant when TIP #239 is widely available
proc ::critcl::preload {args} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
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
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild

    set who [string trim $who]
    if {$who ne ""} {
	set license "This software is copyrighted by $who.\n"
    } else {
	set license ""
    }

    set elicense [common::license-text $args]

    append license $elicense

    # This, 'tsources', 'meta?', and 'meta' are the only places where
    # we are not extending the UUID. Because the license text has no
    # bearing on the binary at all.
    InitializeFile $file

    ImetaSet $file license [common::text2words   $elicense]
    ImetaSet $file author  [common::text2authors $who]
    return
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: meta data (teapot)

proc ::critcl::description {text} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
    InitializeFile $file

    ImetaSet $file description [common::text2words $text]
    return
}

proc ::critcl::summary {text} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
    InitializeFile $file

    ImetaSet $file summary [common::text2words $text]
    return
}

proc ::critcl::subject {args} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
    InitializeFile $file

    ImetaAdd $file subject $args
    return
}

proc ::critcl::meta {key args} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild

    # This, 'meta?', 'license', and 'tsources' are the only places
    # where we are not extending the UUID. Because the meta data has
    # no bearing on the binary at all.
    InitializeFile $file

    dict update v::code($file) config c {
	dict update c meta m {
	    foreach v $args { dict lappend m $key $v }
	}
    }
    return
}

proc ::critcl::meta? {key} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild

    # This, 'meta', 'license', and 'tsources' are the only places
    # where we are not extending the UUID. Because the meta data has
    # no bearing on the binary at all.
    InitializeFile $file

    if {[dict exists $v::code($file) config package $key]} {
	return [dict get $v::code($file) config package $key]
    }
    if {[dict exists $v::code($file) config meta $key]} {
	return [dict get $v::code($file) config meta $key]
    }
    return -code error "Unknown meta data key \"$key\""
}

proc ::critcl::ImetaSet {file key words} {
    dict set v::code($file) config package $key $words
    #puts |||$key|%|[dict get $v::code($file) config package $key]|
    return
}

proc ::critcl::ImetaAdd {file key words} {
    dict update v::code($file) config c {
	dict update c package p {
	    foreach word $words {
		dict lappend p $key $word
	    }
	}
    }
    #puts |||$key|+|[dict get $v::code($file) config package $key]|
    return
}

proc ::critcl::GetMeta {file} {
    if {![dict exists $v::code($file) config meta]} {
	set result {}
    } else {
	set result [dict get $v::code($file) config meta]
    }

    # Merge the package information (= system meta data) with the
    # user's meta data. The system information overrides anything the
    # user may have declared for the reserved keys (name, version,
    # platform, as::author, as::build::date, license, description,
    # summary, require). Note that for the internal bracketing code
    # the system information may not exist, hence the catch. Might be
    # better to indicate the bracket somehow and make it properly
    # conditional.

    #puts %$file

    catch {
	set result [dict merge $result [dict get $v::code($file) config package]]
    }

    return $result
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: user configuration options.

proc ::critcl::userconfig {cmd args} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild
    InitializeFile $file

    if {![llength [info commands ::critcl::UC$cmd]]} {
	return -code error "Unknown method \"$cmd\""
    }

    # Dispatch
    return [eval [linsert $args 0 ::critcl::UC$cmd $file]]
}

proc ::critcl::UCdefine {file oname odesc otype {odefault {}}} {
    # When declared without a default determine one of our own. Bool
    # flag default to true, whereas enum flags, which is the rest,
    # default to their first value.

    # The actual definition ignores the config description. This
    # argument is only used by the static code scanner supporting
    # TEA. See ::critcl::scan::userconfig.

    if {[llength [info level 0]] < 6} {
	set odefault [UcDefault $otype]
    }

    # Validate the default against the type too, before saving
    # everything.
    UcValidate $oname $otype $odefault

    uuid::add $file .uc-def [list $oname $otype $odefault]

    dict set v::code($file) config userflag $oname type    $otype
    dict set v::code($file) config userflag $oname default $odefault
    return
}

proc ::critcl::UCset {file oname value} {
    # NOTE: We can set any user flag we choose, even if not declared
    # yet. Validation of the value happens on query, at which time the
    # flag must be declared.

    dict set v::code($file) config userflag $oname value $value
    return
}

proc ::critcl::UCquery {file oname} {
    # Prefer cached data. This is known as declared, defaults merged,
    # validated.
    if {[dict exists $v::code($file) config userflag $oname =]} {
	return [dict get $v::code($file) config userflag $oname =]
    }

    # Reject use of undeclared user flags.
    if {![dict exists $v::code($file) config userflag $oname type]} {
	error "Unknown user flag \"$oname\""
    }

    # Check if a value was supplied by the calling app. If not, fall
    # back to the declared default.

    if {[dict exists $v::code($file) config userflag $oname value]} {
	set value [dict get $v::code($file) config userflag $oname value]
    } else {
	set value [dict get $v::code($file) config userflag $oname default]
    }

    # Validate value against the flag's type.
    set otype [dict get $v::code($file) config userflag $oname type]
    UcValidate $oname $otype $value

    # Fill cache
    dict set v::code($file) config userflag $oname = $value
    return $value
}

proc ::critcl::UcValidate {oname otype value} {
    switch -exact -- $otype {
	bool {
	    if {![string is bool -strict $value]} {
		error "Expected boolean for user flag \"$oname\", got \"$value\""
	    }
	}
	default {
	    if {[lsearch -exact $otype $value] < 0} {
		error "Expected one of [linsert [join $otype {, }] end-1 or] for user flag \"$oname\", got \"$value\""
	    }
	}
    }
}

proc ::critcl::UcDefault {otype} {
    switch -exact -- $otype {
	bool {
	    return 1
	}
	default {
	    return [lindex $otype 0]
	}
    }
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: API (stubs) management

proc ::critcl::api {cmd args} {
    set file [SkipIgnored [who::is]]
    AbortWhenCalledAfterBuild

    if {![llength [info commands ::critcl::API$cmd]]} {
	return -code error "Unknown method \"$cmd\""
    }

    # Dispatch
    return [eval [linsert $args 0 ::critcl::API$cmd $file]]
}

proc ::critcl::APIscspec {file scspec} {
    uuid::add $file .api-scspec $scspec
    dict set v::code($file) config api_scspec $scspec
    return
}

proc ::critcl::APIimport {file name version} {

    # First we request the imported package, giving it a chance to
    # generate the headers searched for in a moment (maybe it was
    # critcl based as well, and generates things dynamically).

    # Note that this can fail, for example in a cross-compilation
    # environment. Such a failure however does not imply that the
    # required API headers are not present, so we can continue.

    catch {
	package require $name $version
    }

    ImetaAdd $file require [list [list $name $version]]

    # Now we check that the relevant headers of the imported package
    # can be found in the specified search paths.

    set cname [string map {:: _} $name]

    set at [API_locate $cname]
    if {$at eq {}} {
	error "Headers for API $name not found"
    } else {
	msg -nonewline " (stubs import $name $version @ $at/$cname)"
    }

    set def [list $name $version]
    uuid::add $file .api-import $def
    dict update v::code($file) config c {
	dict lappend c api_use $def
    }

    # At last look for the optional .decls file. Ignore if there is
    # none. Decode and return contained stubs table otherwise.

    set decls $at/$cname/$cname.decls
    if {[file exists $decls]} {
	package require stubs::reader
	set T [stubs::container::new]
	stubs::reader::file T $decls
	return $T
    }
    return
}

proc ::critcl::APIexport {file name} {
    msg -nonewline " (stubs export $name)"

    uuid::add $file .api-self $name
    return [dict set v::code($file) config api_self $name]
}

proc ::critcl::APIheader {file args} {
    uuid::add $file .api-headers $args
    return [SetParam api_hdrs $args]
}

proc ::critcl::APIextheader {file args} {
    uuid::add $file .api-eheaders $args
    return [SetParam api_ehdrs $args 0]
}

proc ::critcl::APIfunction {file rtype name arguments} {
    package require stubs::reader

    # Generate a declaration as it would have come straight out of the
    # stubs reader. To this end we generate a C code fragment as it
    # would be have been written inside of a .decls file.

    # TODO: We should record this as well, and later generate a .decls
    # file as part of the export. Or regenerate it from the internal
    # representation.

    if {[llength $arguments]} {
	foreach {t a} $arguments {
	    lappend ax "$t $a"
	}
    } else {
	set ax void
    }
    set decl [stubs::reader::ParseDecl "$rtype $name ([join $ax ,])"]

    uuid::add $file .api-fun $decl
    dict update v::code($file) config c {
	dict lappend c api_fun $decl
    }
    return
}

proc ::critcl::API_locate {name} {
    foreach dir [SystemIncludePaths [who::is]] {
	if {[API_at $dir $name]} { return $dir }
    }
    return {}
}

proc ::critcl::API_at {dir name} {
    foreach suffix {
	Decls.h StubLib.h
    } {
	if {![file exists [file join $dir $name $name$suffix]]} { return 0 }
    }
    return 1
}

proc ::critcl::API_setup {file} {
    package require stubs::gen

    lassign [API_setup_import $file] iprefix idefines
    dict set v::code($file) result apidefines $idefines

    append prefix $iprefix
    append prefix [API_setup_export $file]

    # Save prefix to result dictionary for pickup by Compile.
    if {$prefix eq ""} return

    dict set v::code($file) result apiprefix  $prefix\n
    return
}

proc ::critcl::API_setup_import {file} {
    if {![dict exists $v::code($file) config api_use]} {
	return ""
    }

    #msg -nonewline " (stubs import)"

    set prefix ""
    set defines {}

    foreach def [dict get $v::code($file) config api_use] {
	lassign $def iname iversion

	set cname   [string map {:: _} $iname]
	set upname  [string toupper  $cname]
	set capname [stubs::gen::cap $cname]

	set import [at::here!][subst -nocommands {
	    /* Import API: $iname */
	    #define USE_${upname}_STUBS 1
	    #include <$cname/${cname}Decls.h>
	}]
	append prefix \n$import
	ccode $import

	# TODO :: DOCUMENT environment of the cinit code.
	cinit [subst -nocommands {
	    if (!${capname}_InitStubs (ip, "$iversion", 0)) {
		return TCL_ERROR;
	    }
	}] [subst -nocommands {
	    #include <$cname/${cname}StubLib.h>
	}]

	lappend defines -DUSE_${upname}_STUBS=1
    }

    return [list $prefix $defines]
}

proc ::critcl::API_setup_export {file} {
    if {![dict exists $v::code($file) config api_hdrs] &&
	![dict exists $v::code($file) config api_ehdrs] &&
	![dict exists $v::code($file) config api_fun]} return

    if {[dict exists $v::code($file) config api_self]} {
	# API name was declared explicitly
	set ename [dict get $v::code($file) config api_self]
    } else {
	# API name is implicitly defined, is package name.
	set ename [dict get $v::code($file) config package name]
    }

    set prefix ""

    #msg -nonewline " (stubs export)"

    set cname   [string map {:: _} $ename]
    set upname  [string toupper  $cname]
    set capname [stubs::gen::cap $cname]

    set import [at::here!][subst -nocommands {
	/* Import our own exported API: $ename, mapping disabled */
	#undef USE_${upname}_STUBS
	#include <$cname/${cname}Decls.h>
    }]
    append prefix \n$import
    ccode $import

    # Generate the necessary header files.

    append sdecls "\#ifndef ${cname}_DECLS_H\n"
    append sdecls "\#define ${cname}_DECLS_H\n"
    append sdecls "\n"
    append sdecls "\#include <tcl.h>\n"

    if {[dict exists $v::code($file) config api_ehdrs]} {
	append sdecls "\n"
	foreach hdr [dict get $v::code($file) config api_ehdrs] {
	    append sdecls "\#include \"[file tail $hdr]\"\n"
	}
    }

    if {[dict exists $v::code($file) config api_hdrs]} {
	append sdecls "\n"
	foreach hdr [dict get $v::code($file) config api_hdrs] {
	    set hfile [file tail $hdr]
	    cache::copy2 $hdr $cname/$hfile
	    append sdecls "\#include \"$hfile\"\n"
	}
    }

    # Insert code to handle the storage class settings on Windows.

    append sdecls [string map \
		       [list @cname@ $cname @up@ $upname] \
		       $v::storageclass]

    package require stubs::container
    package require stubs::reader
    package require stubs::gen
    package require stubs::gen::header
    package require stubs::gen::init
    package require stubs::gen::lib
    package require stubs::writer

    # Implied .decls file. Not actually written, only implied in the
    # stubs container invocations, as if read from such a file.

    set T [stubs::container::new]
    stubs::container::library   T $ename
    stubs::container::interface T $cname

    if {[dict exists $v::code($file) config api_scspec]} {
	stubs::container::scspec T \
	    [dict get $v::code($file) config api_scspec]
    }

    if {[dict exists $v::code($file) config api_fun]} {
	set index 0
	foreach decl [dict get $v::code($file) config api_fun] {
	    #puts D==|$decl|
	    stubs::container::declare T $cname $index generic $decl
	    incr index
	}
	append sdecls "\n"
	append sdecls [stubs::gen::header::gen $T $cname]
    } 

    append sdecls "\#endif /* ${cname}_DECLS_H */\n"

    set comment "/* Stubs API Export: $ename */"

    set    thedecls [stubs::writer::gen $T]
    set    slib     [stubs::gen::lib::gen $T]
    set    sinitstatic "  $comment\n  "
    append sinitstatic [stubs::gen::init::gen $T]

    set pn [dict get $v::code($file) config package name]
    set pv [dict get $v::code($file) config package version]

    set    sinitrun $comment\n
    append sinitrun "Tcl_PkgProvideEx (ip, \"$pn\", \"$pv\", (ClientData) &${cname}Stubs);"

    # Save the header files to the result cache for pickup (importers
    # in mode "compile & run", or by the higher-level code doing a
    # "generate package")

    cache::write $cname/${cname}Decls.h   $sdecls
    cache::write $cname/${cname}StubLib.h $slib
    cache::write $cname/${cname}.decls    $thedecls

    dict update v::code($file) result r {
	dict lappend r apiheader [cache::get $cname]
    }

    cinit $sinitrun $sinitstatic
    cflags -DBUILD_$cname

    return $prefix
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Introspection

proc ::critcl::check {args} {
    set file [SkipIgnored [who::is] 0]
    AbortWhenCalledAfterBuild

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
    set file [SkipIgnored [who::is] 0]
    AbortWhenCalledAfterBuild

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
    SkipIgnored [who::is] 1
    AbortWhenCalledAfterBuild
    return 0
}

proc ::critcl::compiling {} {
    SkipIgnored [who::is] 0
    AbortWhenCalledAfterBuild
    # Check that we can indeed run a compiler
    # Should only need to do this if we have to compile the code?

    set v::compiling [HasCompiler]
    return $v::compiling
}

proc ::critcl::done {} {
    set file [SkipIgnored [who::is] 1]
    return [expr {[info exists  v::code($file)] &&
		  [dict exists $v::code($file) result closed]}]
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
## Runtime support to handle the possibility of a prebuilt package using
## the .tcl file with embedded C as its own companon defining regular
## Tcl code for the package as well. If the critcl package is loaded
## already this will cause it to ignore the C definitions, with best
## guesses for failed, done, load, check, compiled, and compiling.

proc ::critcl::Ignore {f} {
    set v::ignore([file normalize $f]) .
    return
}

proc ::critcl::SkipIgnored {f {result {}}} {
    if {[info exists v::ignore($f)]} { return -code return $result }
    return $f
}

# # ## ### ##### ######## ############# #####################
## Implementation -- API: Build Management

proc ::critcl::config {option args} {
    if {![info exists v::options($option)] || [llength $args] > 1} {
	error "option must be one of: [lsort [array names v::options]]"
    }
    if {![llength $args]} {
	if {$option eq "lines"} {
	    return [at::enabled]
	}
	return $v::options($option)
    }

    set newvalue [lindex $args 0]
    if {$option eq "lines"} {
	at::enable $newvalue
    } else {
	set v::options($option) $newvalue
    }
    return $newvalue
}

proc ::critcl::debug {args} {
    # Replace 'all' everywhere, and squash duplicates, whether from
    # this, or user-specified.
    set args [string map {all {memory symbols}} $args]
    set args [lsort -unique $args]

    foreach arg $args {
	switch -- $arg {
	    memory  { foreach x [getconfigvalue debug_memory]  { cflags $x } }
	    symbols { foreach x [getconfigvalue debug_symbols] { cflags $x }
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

proc ::critcl::readconfig {config} {
    variable run
    variable configfile $config

    set cfg [open $config]
    set knowntargets [list]
    set cont ""
    set whenplat ""

    interp eval $run set platform $v::buildplatform

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
		interp eval $run $line
		continue
	    }

	    # (3)
	    if {$plat eq "#"} continue

	    # (4), or (5).
	    if {[lsearch -exact $v::configvars $plat] != -1} {
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
		    ( [string match ${v::buildplatform}* $plat] ||
		      [string match ${plat}* $v::buildplatform] )} {
		    set res ""
		    catch {
			set res [interp eval $run expr $cmd]
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
                    set v::xtargets($plat) $cmd
                }
                copy {
                    # (4c) copy an existing config
                    # XXX - should we error out if no definitions exist
                    # for parent platform config
                    # $cmd contains the parent platform
                    foreach {key val} [array get v::toolchain "$cmd,*"] {
                        set key [lindex [split $key ,] 1]
                        set v::toolchain($plat,$key) $val
                    }
                }
                default {
                    set v::toolchain($plat,$type) $cmd
                }
	    }
	}
    }
    set knowntargets [lsort -unique $knowntargets]
    close $cfg

    # Config file processing has completed.
    # Now select the platform to configure the
    # compiler backend with.

    set v::knowntargets $knowntargets

    # The config file may have selected a configuration based on the
    # TRUE when conditions. Which were matched to v::buildplatform,
    # making the chosen config a variant of it. If that did not happen
    # a platform is chosen from the set of defined targets.
    if {$whenplat ne ""} {
	set match [list $whenplat]
    } else {
	set match [critcl::chooseconfig $v::buildplatform]
    }

    # Configure the backend.

    setconfig ""    ;# defaults
    if {[llength $match]} {
	setconfig [lindex $match 0]
    } else {
	setconfig $v::buildplatform
    }
    return
}

proc ::critcl::chooseconfig {targetconfig {err 0}} {
    # first try to match exactly
    set match [lsearch -exact -all -inline $v::knowntargets $targetconfig]

    # on failure, try to match as glob pattern
    if {![llength $match]} {
        set match [lsearch -glob -all -inline $v::knowntargets $targetconfig]
    }

    # on failure, error out if requested
    if {![llength $match] && $err} {
	error "unknown target $targetconfig - use one of $v::knowntargets"
    }
    return $match
}

proc ::critcl::showconfig {{fd ""}} {
    variable run
    variable configfile

    # XXX replace gen - v::buildplatform
    # XXX Do not use v::targetplatform here. Use v::config.
    # XXX Similarly in setconfig.

    set gen $v::buildplatform
    if {$v::targetplatform eq ""} {
	set plat "default"
    } else {
	set plat $v::targetplatform
    }
    set out [list]
    if {$plat eq $gen} {
	lappend out "Config: $plat"
    } else {
	lappend out "Config: $plat (built on $gen)"
    }
    lappend out "Origin: $configfile"
    lappend out "    [format %-15s cache] [critcl::cache]"
    foreach var [lsort $v::configvars] {
	set val [getconfigvalue $var]
	set line "    [format %-15s $var]"
	foreach word [split [string trim $val]] {
	    if {[set word [string trim $word]] eq ""} continue
	    if {[string length "$line $word"] > 70} {
		lappend out "$line \\"
		set line "    [format %-15s { }] $word"
	    } else {
		set line "$line $word"
	    }
	}
	lappend out $line
    }
    # Tcl variables
    set vars [list]
    set max 0
    foreach idx [array names v::toolchain $v::targetplatform,*] {
	set var [lindex [split $idx ,] 1]
	if {[set len [string length $var]] > $max} {
	    set max $len
	}
	if {$var ne "when" && ![info exists c::$var]} {
	    lappend vars $idx $var
	}
    }
    if {[llength $vars]} {
	lappend out "Tcl variables:"
	foreach {idx var} $vars {
	    set val $v::toolchain($idx)
	    if {[llength $val] == 1} {
		# for when someone inevitably puts quotes around
		# values - e.g. "Windows NT"
		set val [lindex $val 0]
	    }
	    lappend out "    [format %-${max}s $var] $val"
	}
    }
    set out [join $out \n]
    if {$fd ne ""} {
	puts $fd $out
    } else {
	return $out
    }
}

proc ::critcl::showallconfig {{ofd ""}} {
    variable configfile
    set txt [common::cat $configfile]
    if {$ofd ne ""} {
	puts $ofd $txt
    } else {
	return $txt
    }
}

proc ::critcl::setconfig {targetconfig} {
    set v::targetconfig   $targetconfig

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

    set v::targetplatform $targetconfig
    foreach p {gcc cc_r xlc xlc_r cc cl} {
	if {[regsub -- "-$p\$" $v::targetplatform {} v::targetplatform]} break
    }

    set c::platform     ""
    set c::sharedlibext ""

    foreach var $v::configvars {
	if {[info exists v::toolchain($targetconfig,$var)]} {

	    set c::$var $v::toolchain($targetconfig,$var)

	    if {$var eq "platform"} {
		set px [getconfigvalue platform]
		set v::targetplatform [lindex $px 0]
		set v::version        [lindex $px 1]
	    }
	}
    }
    if {[info exists ::env(CFLAGS)]} {
	variable c::compile
	append   c::compile      " $::env(CFLAGS)"
    }
    if {[info exists ::env(LDFLAGS)]} {
	variable c::link
	append   c::link         " $::env(LDFLAGS)"
	append   c::link_preload " $::env(LDFLAGS)"
    }
    if {[string match $v::targetplatform $v::buildplatform]} {
	# expand platform to match host if it contains wildcards
	set v::targetplatform $v::buildplatform
    }
    if {$c::platform eq ""} {
	# default config platform (mainly for the "show" command)
	set c::platform $v::targetplatform
    }
    if {$c::sharedlibext eq ""} {
	set c::sharedlibext [info sharedlibextension]
    }

    cache::def [file join ~ .critcl $v::targetplatform]

    #  set any Tcl variables
    foreach idx [array names v::toolchain $v::targetplatform,*] {
	set var [lindex [split $idx ,] 1]
	if {![info exists c::$var]} {
	    set val $v::toolchain($idx)
	    if {[llength $val] == 1} {
		# for when someone inevitably puts quotes around
		# values - e.g. "Windows NT"
		set val [lindex $val 0]
	    }
	    set $var $val
	}
    }
    return
}

proc ::critcl::getconfigvalue {var} {
    variable run
    if {[catch {set val [interp eval $run [list subst [set c::$var]]]}]} {
	set val [set c::$var]
    }
    return $val
}

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

proc ::critcl::crosscheck {} {
    variable run
    global tcl_platform

    if {$tcl_platform(platform) eq "windows"} {
	set null NUL:
    } else {
	set null /dev/null
    }

    if {![catch {
	set     cmd [linsert $c::version 0 exec]
	lappend cmd 2> $null;#@stdout
	set config [interp eval $run $cmd]
    } msg]} {
	set host ""
	set target ""
	foreach line $config {
	    foreach arg [split $line] {
		if {[string match "--*" $arg]} {
		    lassign [split [string trim $arg -] =] cfg val
		    set $cfg $val
		}
	    }
	}
	if {$host ne $target && [info exists v::xtargets($target)]} {
	    setconfig $target
	    print stderr "Cross compiling using $target"
	}
	# XXX host != target, but not know as config ?
	# XXX Currently ignored.
	# XXX Throwing an error better ?
    }
    return
}

# See (XX) at the end of the file (package state variable setup)
# for explanations of the exact differences between these.

proc ::critcl::knowntargets {} {
    return $v::knowntargets
}

proc ::critcl::targetconfig {} {
    return $v::targetconfig
}

proc ::critcl::targetplatform {} {
    return $v::targetplatform
}

proc ::critcl::buildplatform {} {
    return $v::buildplatform
}

proc ::critcl::actualtarget {} {
    # Check if the chosen target is a cross-compile target.  If yes,
    # we return the actual platform identifier of the target. This is
    # used to select the proper platform director names in the critcl
    # cache, generated packages, when searching for preload libraries,
    # etc. Whereas the chosen target provides the proper compile
    # configuration which will invoke the proper cross-compiler, etc.

    if {[info exists v::xtargets($v::targetplatform)]} {
	return $v::xtargets($v::targetplatform)
    } else {
	return $v::targetplatform
    }
}

proc ::critcl::sharedlibext {} {
    return [getconfigvalue sharedlibext]
}

proc ::critcl::buildforpackage {{buildforpackage 1}} {
    set v::buildforpackage $buildforpackage
    return
}

proc ::critcl::fastuuid {} {
    set v::uuidcounter 1 ;# Activates it.
    return
}

proc ::critcl::cbuild {file {load 1}} {
    if {[info exists v::code($file,failed)] && !$load} {
	set v::buildforpackage 0
	return $v::code($file,failed)
    }

    StatusReset

    # Determine if we should place stubs code into the generated file.
    set placestubs [expr {!$v::buildforpackage}]

    # Determine the requested mode and reset for next call.
    set buildforpackage $v::buildforpackage
    set v::buildforpackage 0

    if {$file eq ""} {
	set file [who::is]
    }

    # NOTE: The 4 pieces of data just below has to be copied into the
    # result even if the build and link-steps are suppressed. Because
    # the load-step must have this information.

    set shlib    [DetermineShlibName $file]
    set initname [DetermineInitName  $file [expr {$buildforpackage ? "ns" : ""}]]

    dict set v::code($file) result tsources   [GetParam $file tsources]
    dict set v::code($file) result mintcl     [MinTclVersion $file]

    if {$v::options(force) || ![file exists $shlib]} {
	LogOpen $file
	set base   [BaseOf              $file]
	set object [DetermineObjectName $file]

	API_setup $file

	# Generate the main C file
	CollectEmbeddedSources $file $base.c $object $initname $placestubs

	# Set the marker for critcl::done and its user, AbortWhenCalledAfterBuild.
	dict set v::code($file) result closed mark

	# Compile main file
        lappend objects [Compile $file $file $base.c $object]

	# Compile the companion C sources as well, if there are any.
        foreach src [GetParam $file csources] {
	    lappend objects [Compile $file $src $src \
				 [CompanionObject $src]]
	}

	# NOTE: The data below has to be copied into the result even
	# if the link-step is suppressed. Because the application
	# (mode 'generate package') must have this information to be
	# able to perform the final link.

	lappendlist objects [GetParam $file cobjects]

	dict set v::code($file) result clibraries [GetParam $file clibraries]
	dict set v::code($file) result ldflags    [GetParam $file ldflags]
	dict set v::code($file) result objects    $objects
	dict set v::code($file) result tk         [UsingTk  $file]
	dict set v::code($file) result preload    [GetParam $file preload]
	dict set v::code($file) result license    [GetParam $file license <<Undefined>>]
	dict set v::code($file) result log        {}
	dict set v::code($file) result meta       [GetMeta $file]

	# Link and load steps.
        if {$load || !$buildforpackage} {
	    Link $file
	}

	set msgs [LogClose]

	dict set v::code($file) result warnings [CheckForWarnings $msgs]
    }

    if {$v::failed} {
	if {!$buildforpackage} {
	    print stderr "$msgs\ncritcl build failed ($file)"
	} else {
	    dict set v::code($file) result log $msgs
	}
    } elseif {$load && !$buildforpackage} {
	Load $file
    }

    # Release the data which was collected for the just-built file, as
    # it is not needed any longer.
    dict unset v::code($file) config
    uuid::clear $file

    return [StatusSave $file]
}

proc ::critcl::cresults {{file {}}} {
    if {$file eq ""} { set file [who::is] }
    return [dict get $v::code($file) result]
}

proc ::critcl::cnothingtodo {f} {
    # No critcl definitions at all ?
    if {![info exists  v::code($f)]} { return 1 }

    # We have results already, so where had been something to do.
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

    # Note: We cannot use -directory here. The PATTERN may already be
    # an absolute path, in which case the join will return the
    # unmodified PATTERN to glob on, whereas with -directory the final
    # pattern will be BASE/PATTERN which won't find anything, even if
    # PATTERN actually exists.

    set files {}
    foreach vfile [glob [file join $base $pattern]] {
	set vfile [file normalize $vfile]
	if {![file exists $vfile]} {
	    error "$vfile: not found"
	}
	lappend files $vfile
    }
    return $files
}

proc ::critcl::InitializeFile {file} {
    if {![info exists v::code($file)]} {
	set      v::code($file) {}

	# Initialize the meta data sections (user (meta) and system
	# (package)).

	dict set v::code($file) config meta    {}

	dict set v::code($file) config package platform \
	    [TeapotPlatform]
	dict set v::code($file) config package build::date \
	    [list [clock format [clock seconds] -format {%Y-%m-%d}]]

	# May not exist, bracket code.
	if {![file exists $file]} return

	scan-dependencies $file $file provide
	return
    }

    if {![dict exists $v::code($file) config]} {
	dict set v::code($file) config {}
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
    return [llength [auto_execok [lindex [getconfigvalue compile] 0]]]
}

# # ## ### ##### ######## ############# #####################
## Backend, Direct compiling, linking of test C sources.

proc ::critcl::CompileDirect {file label code {mode temp}} {
    set src [cache::write check_[pid].c $code]
    set obj [file rootname $src][getconfigvalue object]

    # See also the internal helper command 'Compile'. The code here is
    # in essence a simplified form of that.

    set         cmdline [getconfigvalue compile]
    lappendlist cmdline [GetParam $file cflags]
    lappendlist cmdline [SystemIncludes $file]
    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $src

    LogOpen $file
    Log* "${label}... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK FAILED]
    StatusReset

    if {!$ok || ($mode eq "temp")} {
	LogClose
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
    set obj [file rootname $out][getconfigvalue object]

    set cmdline [getconfigvalue link]

    if {$option::debug_symbols} {
	lappendlist cmdline [getconfigvalue link_debug]
    } else {
	lappendlist cmdline [getconfigvalue strip]
	lappendlist cmdline [getconfigvalue link_release]
    }

    lappendlist cmdline [LinkResult $out]
    lappendlist cmdline $obj
    lappendlist cmdline [SystemLibraries]
    lappendlist cmdline [FixLibraries [GetParam $file clibraries]]
    lappendlist cmdline [GetParam $file ldflags]

    Log* "${label} (link)... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK ERR]

    LogClose
    cache::clear check_[pid].*
    return $ok
}

# # ## ### ##### ######## ############# #####################
## Backend Processing, Collected Sources

proc ::critcl::CollectEmbeddedSources {file destination libfile ininame placestubs} {
    set fd [open $destination w]

    if {[dict exists $v::code($file) result apiprefix]} {
	set api [dict get $v::code($file) result apiprefix]
    } else {
	set api ""
    }

    # Boilerplate header.
    puts $fd [subst [common::cat [Template header.c]]]
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
	set stubs     [TclDecls     $file]
	set platstubs [TclPlatDecls $file]
	puts -nonewline $fd [subst [common::cat [Template stubs.c]]]
	#                    ^=> mintcl, stubs, platstubs
    } else {
	# Declarations only, for linking, in the sub-packages.
	puts -nonewline $fd [subst [common::cat [Template stubs_e.c]]]
	#                    ^=> mintcl
    }

    if {[UsingTk $file]} {
	SetupTkStubs $fd
    }

    # Initialization boilerplate. This ends in the middle of the
    # FOO_Init() function, leaving it incomplete.

    set ext [GetParam $file edecls]
    puts $fd [subst [common::cat [Template pkginit.c]]]
    #         ^=> ext, ininame

    # From here on we are completing FOO_Init().
    # Tk setup first, if requested. (Tcl is already done).
    if {[UsingTk $file]} {
	puts $fd [common::cat [Template pkginittk.c]]
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
    puts  $fd [common::cat [Template pkginitend.c]]
    close $fd
    return
}

proc ::critcl::MinTclVersion {file} {
    set required [GetParam $file mintcl 8.4]
    foreach version $v::hdrsavailable {
	if {[package vsatisfies $version $required]} {
	    return $version
	}
    }
    return $required
}

proc ::critcl::UsingTk {file} {
    return [GetParam $file tk 0]
}

proc ::critcl::TclIncludes {file} {
    # Provide access to the Tcl/Tk headers using a -I flag pointing
    # into the critcl package directory hierarchy. No copying of files
    # required. This also handles the case of the X11 headers on
    # windows, for free.

    set hdrs tcl[MinTclVersion $file]
    set path [file join $v::hdrdir $hdrs]

    if {[file system $path] ne "native"} {
	# The critcl package is wrapped. Copy the relevant headers out
	# to disk (cache) and change the include path appropriately.
	set path [cache::copy2 $path]
    }

    return [list $c::include$path]
}

proc ::critcl::TclHeader {file {header {}}} {
    # Provide access to the Tcl/Tk headers in the critcl package
    # directory hierarchy. No copying of files required.
    set hdrs tcl[MinTclVersion $file]
    return [file join $v::hdrdir $hdrs $header]
}

proc ::critcl::SystemIncludes {file} {
    set includes {}
    foreach dir [SystemIncludePaths $file] {
	lappend includes $c::include$dir
    }
    return $includes
}

proc ::critcl::SystemIncludePaths {file} {
    set paths {}
    set has {}

    # critcl -I options.
    foreach dir $v::options(I) {
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
	    # Transform into a -I directory reference.
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
	lappend libincludes $c::libinclude$dir
    }
    return $libincludes
}

proc ::critcl::SystemLibraryPaths {} {
    set paths {}
    set has {}

    # critcl -L options.
    foreach dir $v::options(L) {
	if {[dict exists $has $dir]} continue
	dict set has $dir yes
	lappend paths $dir
    }

    return $paths
}

proc ::critcl::Compile {tclfile origin cfile obj} {
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

    set         cmdline [getconfigvalue compile]
    lappendlist cmdline [GetParam $tclfile cflags]
    lappendlist cmdline [getconfigvalue threadflags]
    if {$v::options(combine) ne "standalone"} {
	lappendlist cmdline [getconfigvalue tclstubs]
    }
    if {$v::options(language) ne "" && [file tail $tclfile] ne "critcl.tcl"} {
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	# See also -x none below.
	lappend cmdline -x $v::options(language)
    }
    lappendlist cmdline [TclIncludes $tclfile]
    lappendlist cmdline [SystemIncludes $tclfile]

    if {[dict exists $v::code($tclfile) result apidefines]} {
	lappendlist cmdline [dict get $v::code($tclfile) result apidefines]
    }

    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $cfile

    if {$v::options(language) ne ""} {
	# Allow the compiler to determine the type of file otherwise
	# it will try to compile the libs
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	lappend cmdline -x none
    }

    # Add the Tk stubs to the command line, if requested and not suppressed
    if {[UsingTk $tclfile] && ($v::options(combine) ne "standalone")} {
	lappendlist cmdline [getconfigvalue tkstubs]
    }

    if {!$option::debug_symbols} {
	lappendlist cmdline [getconfigvalue optimize]
	lappendlist cmdline [getconfigvalue noassert]
    }

    if {[ExecWithLogging $cmdline \
	     {$obj: [file size $obj] bytes} \
	     {ERROR while compiling code in $origin:}]} {
	if {!$v::options(keepsrc) && $cfile ne $origin} {
	    file delete $cfile
	}
    }

    return $obj
}

proc ::critcl::MakePreloadLibrary {file} {
    StatusAbort?

    # compile and link the preload support, if necessary, i.e. not yet
    # done.

    set shlib [cache::get preload[getconfigvalue sharedlibext]]
    if {[file exists $shlib]} return

    # Operate like TclIncludes. Use the template file directly, if
    # possible, or, if we reside in a virtual filesystem, copy it to
    # disk.

    set src [Template preload.c]
    if {[file system $src] ne "native"} {
	set src [cache::copy2 $src]
    }

    # Build the object for the helper package, 'preload' ...

    set obj [cache::get preload.o]
    Compile $file $src $src $obj

    # ... and link it.
    # Custom linker command. XXX Can we bent Link to the task?
    set         cmdline [getconfigvalue link]
    lappend     cmdline $obj
    lappendlist cmdline [getconfigvalue strip]
    lappendlist cmdline [LinkResult $shlib]

    ExecWithLogging $cmdline \
	{$shlib: [file size $shlib] bytes} \
	{ERROR while linking $shlib:}

    # Now the critcl application can pick up this helper shlib and
    # stuff it into the package it is making.
    return
}

proc ::critcl::Link {file} {
    StatusAbort?

    set shlib   [dict get $v::code($file) result shlib]
    set preload [dict get $v::code($file) result preload]

    # Assemble the link command.
    set cmdline [getconfigvalue link]

    if {[llength $preload]} {
	lappendlist cmdline [getconfigvalue link_preload]
    }

    if {$option::debug_symbols} {
	lappendlist cmdline [getconfigvalue link_debug]
    } else {
	lappendlist cmdline [getconfigvalue strip]
	lappendlist cmdline [getconfigvalue link_release]
    }

    lappendlist cmdline [LinkResult $shlib]
    lappendlist cmdline [GetObjects $file]
    lappendlist cmdline [SystemLibraries]
    lappendlist cmdline [GetLibraries $file]
    lappendlist cmdline [dict get $v::code($file) result ldflags]
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

    set em [getconfigvalue embed_manifest]

    critcl::Log "Manifest Command: $em"
    critcl::Log "Manifest File:    [expr {[file exists $shlib.manifest]
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
	MakePreloadLibrary $file
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

    return [cache::get ${srcbase}[getconfigvalue object]]

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
    return [subst $c::output]
}

proc ::critcl::LinkResult {shlib} {
    # Variable used by the subst'able config setting.
    set outfile $shlib

    set ldout [subst $c::ldoutput]
    if {$ldout eq ""} {
	set ldout [subst $c::output]
    }

    return $ldout
}

proc ::critcl::GetObjects {file} {
    # On windows using the native MSVC compiler put the companion
    # object files into a link file to read, instead of separately on
    # the command line.

    set objects [dict get $v::code($file) result objects]

    if {![string match "win32-*-cl" $v::buildplatform]} {
	return $objects
    }

    set rsp [cache::write link.fil \"[join $objects \"\n\"]\"]
    return [list @$rsp]
}

proc ::critcl::GetLibraries {file} {
    # On windows using the native MSVC compiler, transform all -lFOO
    # references into FOO.lib.

    return [FixLibraries [dict get $v::code($file) result clibraries]]
}

proc ::critcl::FixLibraries {libraries} {
    if {[string match "win32-*-cl" $v::buildplatform]} {
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
    puts -nonewline $fd [common::cat [Template tkstubs.c]]
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
    set         cmd [getconfigvalue preproc_define]
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

    set         cmd [getconfigvalue preproc_enum]
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
    if {!$v::options(keepsrc)} {
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

proc ::critcl::Load {f} {
    set shlib [dict get $v::code($f) result shlib]
    set init  [dict get $v::code($f) result initname]
    set tsrc  [dict get $v::code($f) result tsources]
    set minv  [dict get $v::code($f) result mintcl]

    # Using the renamed builtin. While this is a dependency it was
    # recorded already. See 'critcl::tcl', and 'critcl::tk'.
    #package require Tcl $minv
    ::load $shlib $init

    # See the critcl application for equivalent code placing the
    # companion tcl sources into the generated package. Here, for
    # 'compile & run' we now source the companion files directly.
    foreach t $tsrc {
	Ignore $t
	source $t
    }
    return
}

proc ::critcl::AbortWhenCalledAfterBuild {} {
    if {![done]} return
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

# XXX Refactor to avoid duplication of the memoization code.
proc ::critcl::DetermineShlibName {file} {
    # Return cached information, if present.
    if {[info exists  v::code($file)] &&
	[dict exists $v::code($file) result shlib]} {
	return [dict get $v::code($file) result shlib]
    }

    # The name of the shared library we hope to produce (or use)
    set shlib [BaseOf $file][getconfigvalue sharedlibext]

    dict set v::code($file) result shlib $shlib
    return $shlib
}

proc ::critcl::DetermineObjectName {file} {
    # Return cached information, if present.
    if {[info exists  v::code($file)] &&
	[dict exists $v::code($file) result object]} {
	return [dict get $v::code($file) result object]
    }

    set object [BaseOf $file]

    # The generated object file will be saved for permanent use if the
    # outdir option is set (in which case rebuilds will no longer be
    # automatic).
    if {$v::options(outdir) ne ""} {
	set odir [file join [file dirname $file] $v::options(outdir)]
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
    switch -- $v::options(combine) {
	""         -
	dynamic    { append object _pic[getconfigvalue object] }
	static     { append object _stub[getconfigvalue object] }
	standalone { append object [getconfigvalue object] }
    }

    dict set v::code($file) result object $object
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

    dict set v::code($file) result initname $ininame

    catch {
	dict set v::code($file) result pkgname \
	    [dict get $v::code($file) config package name]
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
## Implementation -- Internals - Access to the log file

proc ::critcl::LogOpen {file} {
    set   v::logfile [cache::get [pid].log]
    set   v::log     [open $v::logfile w]
    puts $v::log "\n[clock format [clock seconds]] - $file"
    return
}

proc ::critcl::LogCmdline {cmdline} {
    set w [join [lassign $cmdline cmd] \n\t]
    Log \n$cmd\n\t$w\n
    return
}

proc ::critcl::Log {msg} {
    puts $v::log $msg
    return
}

proc ::critcl::Log* {msg} {
    puts -nonewline $v::log $msg
    return
}

proc ::critcl::LogClose {} {
    # Transfer the log messages for the current file over into the
    # global critcl log, and cleanup.

    close $v::log
    set msgs [common::cat $v::logfile]
    cache::append $v::prefix.log $msgs

    file delete -force $v::logfile
    unset v::log v::logfile
    return $msgs
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - UUID management, change detection

proc ::critcl::BaseOf {f} {
    # Return cached information, if present.
    if {[info exists  v::code($f)] &&
	[dict exists $v::code($f) result base]} {
	return [dict get $v::code($f) result base]
    }

    set base [file normalize [cache::get ${v::prefix}_[uuid::get $f]]]

    dict set v::code($f) result base $base
    return $base
}

# # ## ### ##### ######## ############# #####################
## Implementation -- Internals - Miscellanea

proc ::critcl::Separator {} {
    return "/* [string repeat - 70] */"
}

proc ::critcl::Template {file} {
    variable v::hdrdir
    return [file join $hdrdir $file]
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

proc ::critcl::StatusSave {file} {
    # XXX FUTURE Use '$(file) result failed' later
    set result $v::failed
    set v::code($file,failed) $v::failed
    set v::failed 0
    return $result
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

proc ::critcl::Exec {cmdline} {
    variable run

    set v::failed [catch {
	interp eval $run [linsert $cmdline 0 exec]
    } v::err]

    return [expr {!$v::failed}]
}

proc ::critcl::ExecWithLogging {cmdline okmsg errmsg} {
    variable run

    LogCmdline $cmdline

    # Extend the command, redirect all of its output (stdout and
    # stderr) into the current log.
    lappend cmdline >&@ $v::log

    interp transfer {} $v::log $run

    set ok [Exec $cmdline]

    interp transfer $run $v::log {}

    if {$ok} {
	Log [uplevel 1 [list subst $okmsg]]
    } else {
	Log [uplevel 1 [list subst $errmsg]]
	Log $v::err
    }

    return $ok
}

proc ::critcl::BuildPlatform {} {
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

    # Memoize
    proc ::critcl::BuildPlatform {} [list return $platform]
    return $platform
}

proc ::critcl::IsGCC {path} {
    if {[catch {
	set lines [exec $path -v |& grep gcc]
    }] || ($lines eq {})} { return 0 }
    return 1
}

proc ::critcl::Here {} {
    return [file dirname [who::is]]
}

proc ::critcl::TclDecls {file} {
    return [TclDef $file tclDecls.h tclStubsPtr]
}

proc ::critcl::TclPlatDecls {file} {
    return [TclDef $file tclPlatDecls.h tclPlatStubsPtr]
}

proc ::critcl::TclDef {file hdr var} {
    #puts F|$file
    set hdr [TclHeader $file $hdr]

    if {![file exists   $hdr]} { error "Header file not found: $hdr" }
    if {![file isfile   $hdr]} { error "Header not a file: $hdr" }
    if {![file readable $hdr]} { error "Header not readable: $hdr (no permission)" }

    #puts H|$hdr
    if {[catch {
	set hdrcontent [split [common::cat $hdr] \n]
    } msg]} {
	error "Header not readable: $hdr ($msg)"
    }

    # Note, Danger: The code below is able to use declarations which
    # are commented out in various ways (#if 0, /* ... */, and //
    # ...), because it is performing a simple line-oriented search
    # without context, and not matching against comment syntax either.

    set ext [Grep *extern* $hdrcontent]
    if {![llength $ext]} {
	error "No extern declarations found in $hdr"
    }

    set vardecl [Grep *${var}* $ext]
    if {![llength $vardecl]} {
	error "No declarations for $var found in $hdr"
    }

    set def [string map {extern {}} [lindex $vardecl 0]]
    msg " ($var => $def)"
    return $def
}

proc ::critcl::Grep {pattern lines} {
    set r {}
    foreach line $lines {
	if {![string match $pattern $line]} continue
	lappend r $line
    }
    return $r
}

# # ## ### ##### ######## ############# #####################
## Initialization

proc ::critcl::Initialize {} {
    variable mydir [Here] ; # Path of the critcl package directory.

    variable run              [interp create]
    variable v::buildplatform [BuildPlatform]
    variable v::hdrdir	      [file join $mydir critcl_c]
    variable v::hdrsavailable

    # Scan the directory holding the C fragments and our copies of the
    # Tcl header and determine for which versions of Tcl we actually
    # have headers. This allows distributions to modify the directory,
    # i.e. drop our copies and refer to the system headers instead, as
    # much as are installed, and critcl adapts. The tcl versions are
    # recorded in ascending order, making upcoming searches easier,
    # the first satisfying version is also always the smallest.

    foreach d [lsort -dict [glob -types {d r} -directory $hdrdir -tails tcl*]] {
	lappend hdrsavailable [regsub {^tcl} $d {}]
    }

    # The prefix is based on the package's version. This allows
    # multiple versions of the package to use the same cache without
    # interfering with each. Note that we cannot use 'pid' and similar
    # information, because this would circumvent the goal of the
    # cache, the reuse of binaries whose sources did not change.

    variable v::prefix	"v[package require critcl]"

    regsub -all {\.} $prefix {} prefix

    # keep config options in a namespace
    foreach var $v::configvars {
	set c::$var {}
    }

    # read default configuration. This also chooses and sets the
    # target platform.
    readconfig [file join $mydir Config]

    rename ::critcl::Initialize {}
    return
}

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl {
    variable mydir    ;# Path of the critcl package directory.
    variable run      ;# interpreter to run commands, eval when, etc

    # XXX configfile - See the *config commands, path of last config file run through 'readconfig'.

    # namespace to flag when options set
    namespace eval option {
        variable debug_symbols  0
    }

    # keep all variables in a sub-namespace for easy access
    namespace eval v {
	# ----------------------------------------------------------------

	# (XX) To understand the set of variables below and their
	# differences some terminology is required.
	#
	# First we have to distinguish between "target identifiers"
	# and "platform identifiers". The first is the name for a
	# particular set of configuration settings specifying commands
	# and command line arguments to use. The second is the name of
	# a machine configuration, identifying both operating system,
	# and cpu architecture.
	#
	# The problem critcl has is that in 99% of the cases found in
	# a critcl config file the "target identifier" is also a valid
	# "platform identifier". Example: "linux-ix86". That does not
	# make them semantically interchangable however.
	#
	# Especially when we add cross-compilation to the mix, where
	# we have to further distinguish between the platform critcl
	# itself is running on (build), and the platform for which
	# critcl is generating code (target), and the last one sounds
	# similar to "target identifier".

	variable targetconfig    ;# Target identifier. The chosen configuration.
	variable targetplatform  ;# Platform identifier. We generate binaries for there.
	variable buildplatform   ;# Platform identifier. We run here.

	variable knowntargets {} ;# List of all target identifiers found
	# in the configuration file last processed by "readconfig".
	
	variable xtargets        ;# Cross-compile targets. This array maps from
	array set xtargets {}    ;# the target identifier to the actual platform
	# identifier of the target platform in question. If a target identifier
	# has no entry here, it is assumed to be the platform identifier itself.
	# See "critcl::actualtarget".

	# ----------------------------------------------------------------

	variable version ""      ;# String. Min version number on platform
	variable hdrdir          ;# Path. Directory containing the helper
				  # files of the package. A sub-
				  # directory of 'mydir', see above.
	variable hdrsavailable   ;# List. Of Tcl versions for which we have
	                          # Tcl header files available. For details
	                          # see procedure 'Initialize' above.
	variable prefix          ;# String. The string to start all file names
				  # generated by the package with. See
				  # 'Initialize' for our choice and
				  # explanation of it.
	variable options         ;# An array containing options
				  # controlling the code generator.
				  # For more details see below.
	set options(outdir)   "" ;# - Path. If set the place where the generated
				  #   shared library is saved for permanent use.
	set options(keepsrc)  0  ;# - Boolean. If set all generated .c files are
				  #   kept after compilation. Helps with debugging
				  #   the critcl package.
	set options(combine)  "" ;# - XXX standalone/dynamic/static
				  #   XXX Meaning of combine?
	set options(force)    0  ;# - Boolean. If set (re)compilation is
				  #   forced, regardless of the state of
				  #   the cache.
	set options(I)        "" ;# - List. Additional include
				  #   directories, globally specified by
				  #   the user for mode 'generate
				  #   package', for all components put
				  #   into the package's library.
	set options(L)        "" ;# - List. Additional library search
				  #   directories, globally specified by
				  #   the user for mode 'generate
				  #   package'.
	set options(language) "" ;# - String. XXX
	set options(lines)    -  ;# - See at::enabled. Fake here, for error message.
	                          #   critcl::config properly redirects

	# XXX clientdata() per-command (See ccommand). per-file+ccommand better?
	# XXX delproc()    per-command (See ccommand). s.a

	# XXX toolchain()  <platform>,<configvarname> -> data
	# XXX            Used only in {read,set,show}config.
	# XXX            Seems to be a database holding the total contents of the
	# XXX            config file.

	# knowntargets  - See the *config commands, list of all platforms we can compile for.

	# I suspect that this came later

	variable storageclass {
/*
 * These macros are used to control whether functions are being declared for
 * import or export. If a function is being declared while it is being built
 * to be included in a shared library, then it should have the DLLEXPORT
 * storage class. If is being declared for use by a module that is going to
 * link against the shared library, then it should have the DLLIMPORT storage
 * class. If the symbol is beind declared for a static build or for use from a
 * stub library, then the storage class should be empty.
 *
 * The convention is that a macro called BUILD_xxxx, where xxxx is the name of
 * a library we are building, is set on the compile line for sources that are
 * to be placed in the library. When this macro is set, the storage class will
 * be set to DLLEXPORT. At the end of the header file, the storage class will
 * be reset to DLLIMPORT.
 */

#undef TCL_STORAGE_CLASS
#ifdef BUILD_@cname@
#   define TCL_STORAGE_CLASS DLLEXPORT
#else
#   ifdef USE_@up@_STUBS
#      define TCL_STORAGE_CLASS
#   else
#      define TCL_STORAGE_CLASS DLLIMPORT
#   endif
#endif
	}

	variable code	         ;# This array collects all code snippets and
				  # data about them.

	# Keys for 'code' (above) and their contents:
	#
	# <file> -> Per-file information, nested dictionary. Sub keys:
	#
	#	result		- Results needed for 'generate package'.
	#		initname	- String. Foo in Foo_Init().
	#		tsources	- List. The companion tcl sources for <file>.
	#		object		- String. Name of the object file backing <file>.
	#		objects		- List. All object files, main and companions.
	#		shlib		- String. Name of the shared library backing <file>.
	#		base		- String. Common prefix (file root) of 'object' and 'shlib'.
	#		clibraries	- List. See config. Copy for global linkage.
	#		ldflags		- List. See config. Copy for global linkage.
	#		mintcl		- String. Minimum version of Tcl required by the package.
	#		preload		- List. Names of all libraries to load before the package library.
	#		license		- String. License text.
	#	<= "critcl::cresults"
	#
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
	#		uuid		- List. Strings used to generate the file's uuid/hash.
	#		mintcl		- String. Minimum version of Tcl required by the package.
	#		preload		- List. Names of all libraries to load
	#				  before the package library. This
	#				  information is used only by mode
	#				  'generate package'. This means that
	#				  packages with preload can't be used
	#				  in mode 'compile & run'.
	#		license		- String. License text.
	#		api_self	- String. Name of our API. Defaults to package name.
	#		api_hdrs	- List. Exported public headers of the API.
	#		api_ehdrs	- List. Exported external public headers of the API.
	#		api_fun		- List. Exported functions (signatures of result type, name, and arguments (C syntax))
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
	# <file>,failed -> Per-file information: Boolean. Build status. Failed or not.
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

	variable log     ""      ;# Log channel, opened to logfile.
	variable logfile ""      ;# Path of logfile. Accessed by
				  # "Log*" and "ExecWithLogging".
	variable failed  0       ;# Build status. Used by "Status*"
	variable err     ""	 ;# and "Exec*". Build error text.

	variable uuidcounter 0   ;# Counter for uuid generation in package mode.
	                         ;# md5 is bypassed when used.

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

	# _____________________________________________________________________

	variable compiling 0     ;# Boolean. Indicates that a C compiler
				  # (gcc, native, cl) is available.

	# _____________________________________________________________________
	# config variables
	variable configvars {
	    compile
	    debug_memory
	    debug_symbols
	    include
	    libinclude
	    ldoutput
	    embed_manifest
	    link
	    link_debug
	    link_preload
	    link_release
	    noassert
	    object
	    optimize
	    output
	    platform
	    preproc_define
	    preproc_enum
	    sharedlibext
	    strip
	    tclstubs
	    threadflags
	    tkstubs
	    version
	}
    }

    # namespace holding the compiler configuration (commands and
    # options for the various tasks, i.e. compilation, linking, etc.).
    namespace eval c {
	# See sibling file 'Config' for the detailed and full
	# information about the variables in use. configvars above, and
	# the code below list only the variables relevant to C. Keep this
	# information in sync with the contents of 'Config'.

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

::critcl::Initialize
return
