# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of per-file stubs table information.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::cache  ;# Access to result cache
package require critcl::cdefs  ;# General collected C definitions.
package require critcl::common ;# General critcl utilities.
package require critcl::meta   ;# Management of teapot meta data.
package require critcl::uuid   ;# Digesting, change detection.
package require critcl::tags   ;# Management of indicator flags.
package require debug          ;# debug narrative

package provide critcl::api 4

namespace eval ::critcl::api {
    namespace export c_scspec c_import c_export \
	c_header c_extheader c_function \
	complete
    catch { namespace ensemble create }
}

debug level  critcl/api
debug prefix critcl/api {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## Link the lifetime of stubs table information to the general C
## definitions for a context.

cdefs on-clear ::critcl::api::clear

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::api::c_scspec {context scspec} {
    debug.critcl/api {}
    variable scspec
    cdefs initialize $context

    uuid add $context .api-scspec $scspec
    dict set scspec $context $scspec
    return
}

proc ::critcl::api::c_import {context name version} {
    debug.critcl/api {}
    variable use
    cdefs initialize $context

    # First we request the imported package, giving it a chance to
    # generate the headers searched for in a moment (maybe it was
    # critcl based as well, and generates things dynamically).

    # Note that this can fail, for example in a cross-compilation
    # environment. Such a failure however does not imply that the
    # required API headers are not present, so we can continue.

    catch {
	package require $name $version
    }

    meta require $context [list $name $version]

    # Now we check that the relevant headers of the imported package
    # can be found in the specified search paths.

    set cname [string map {:: _} $name]

    set at [LocateDecls $context $cname]
    if {$at eq {}} {
	::critcl::error "Headers for API $name not found"
    } else {
	::critcl::msg -nonewline " (stubs import $name $version @ $at/$cname)"
    }

    set def [list $name $version]
    uuid add $context .api-import $def
    dict lappend use $context $def

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

proc ::critcl::api::c_export {context name} {
    debug.critcl/api {}
    variable self
    cdefs initialize $context
    ::critcl::msg -nonewline " (stubs export $name)"

    uuid add $context .api-self $name
    dict set self $context $name
    return $name
}

proc ::critcl::api::c_header {context args} {
    debug.critcl/api {}
    variable hdrs
    cdefs initialize $context

    set base [file dirname $context]

    uuid add $context .api-headers $args
    foreach pattern $args {
	foreach v [common expand-glob $base $pattern] {
	    dict lappend hdrs $context $v
	}
    }
    return
}

proc ::critcl::api::c_extheader {context args} {
    debug.critcl/api {}
    variable ehdrs
    cdefs initialize $context

    uuid add $context .api-eheaders $args
    foreach v $args {
	dict lappend ehdrs $context $v
    }
    return
}

proc ::critcl::api::c_function {context rtype name arguments} {
    debug.critcl/api {}
    variable fun
    cdefs initialize $context

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
    uuid add $context .api-fun $decl
    dict lappend fun $context $decl
    return
}

proc ::critcl::api::clear {context} {
    debug.critcl/api {}
    variable scspec ; dict unset scspec $context
    variable self   ; dict unset self   $context
    variable hdrs   ; dict unset hdrs   $context
    variable ehdrs  ; dict unset ehdrs  $context
    variable fun    ; dict unset fun    $context
    variable use    ; dict unset use    $context

    # See api::complete for where these are set/created.
    tags unset $context apidefines
    tags unset $context apiprefix
    #tags unset $context apiheader - XXX lifetime beyond build (app-critcl)
    # XXX FIXME apiheader destruction in compile&run mode
    return
}

proc ::critcl::api::complete {context} {
    debug.critcl/api {}
    # Generate various pieces of C code needed to import the used
    # stubs tables into the package.

    set flags   {} ;# list: cc flags to build with stubs.
    set defines {} ;# list: C defines activating use of imported stubs.
    set code    {} ;# list: C fragments for including stubs decl headers.
    set decls   {} ;# list: C fragments for including stubs lib headers.
                    #       (in lieu of a libXXXstub.a).
    set init    {} ;# list: C fragments for calling stubs init functions

    CompleteImport $context
    set cname [CompleteExport $context]

    foreach i $init d $decls {
	cdefs init $i $d
    }
    foreach import $code {
	cdefs code $import ;# (**) cc.tcl
    }

    tags set $context apidefines $defines

    if {[llength $code]} {
	tags set $context apiprefix \n[join $code \n]\n
	tags set $context apiheader [cache get $cname]
    } else {
	tags set $context apiprefix {}
	tags set $context apiheader {}
    }

    return [list $flags]
}

proc ::critcl::api::CompleteImport {context} {
    debug.critcl/api {}
    variable use
    if {![dict exists $use $context]} return

    upvar 1 defines defines code code decls decls init init

    #::critcl::msg -nonewline " (stubs import completion)"

    package require stubs::gen

    foreach def [dict get $use $context] {
	lassign $def iname iversion

	lappend code [StubUse $iname \
			  "Import API: $iname" \
			  cname capname upname]

	# TODO :: DOCUMENT environment of the cinit code.
	lappend init [subst -nocommands {
	    if (!${capname}_InitStubs (ip, "$iversion", 0)) {
		return TCL_ERROR;
	    }
	}]
	lappend decls [subst -nocommands {
	    #include <$cname/${cname}StubLib.h>
	}]

	lappend defines -DUSE_${upname}_STUBS=1
    }

    return
}

proc ::critcl::api::CompleteExport {context} {
    debug.critcl/api {}
    variable hdrs
    variable ehdrs
    variable fun

    if {![dict exists $hdrs  $context] &&
	![dict exists $ehdrs $context] &&
	![dict exists $fun   $context]} return

    #::critcl::msg -nonewline " (stubs export completion)"

    variable self
    upvar 1 flags flags code code decls decls init init

    if {[dict exists $self $context]} {
	# API name was declared explicitly
	set ename [dict get $self $context]
    } else {
	# API name is implicitly defined, as the package name.
	set ename [meta gets $context name]
    }

    lappend code [StubUse $ename \
		      "Import our own exported API: $ename, mapping disabled" \
		      cname capname upname]

    StubDeclHeader    sdecls          $cname
    StubDeclExternals sdecls $context
    StubDeclPublic    sdecls $context $cname
    StubDeclStorage   sdecls          $cname $upname
    StubDeclDef       sdecls $context $cname $ename  T
    StubDeclTrailer   sdecls $cname

    cache write $cname/${cname}Decls.h   $sdecls

    set comment "/* Stubs API Export: $ename */"

    lappend init  [StubDeclInitCode $context $cname $comment]
    lappend decls [StubDeclInitDecl $T $comment]
    lappend flags -DBUILD_$cname

    # Save the header files to the result cache for pickup (importers
    # in mode "compile & run", or by the higher-level code doing a
    # "generate package")

    cache write $cname/${cname}.decls    [stubs::writer::gen   $T]
    cache write $cname/${cname}StubLib.h [stubs::gen::lib::gen $T]

    return $cname
}

proc ::critcl::api::StubDeclInitDecl {T comment} {
    set    sinitstatic "  $comment\n  "
    append sinitstatic [stubs::gen::init::gen $T]

    return $sinitstatic
}

proc ::critcl::api::StubDeclInitCode {context cname comment} {
    debug.critcl/api {}
    set pn [meta gets $context name]
    set pv [meta gets $context version]

    append sinitrun $comment
    append sinitrun \n
    append sinitrun "Tcl_PkgProvideEx (ip, \"$pn\", \"$pv\", (ClientData) &${cname}Stubs);"

    return $sinitrun
}

proc ::critcl::api::StubDeclHeader {sv cname} {
    debug.critcl/api {}
    upvar 1 $sv sdecls
    # Standard heading for a *Decls.h header file.

    append sdecls "\#ifndef ${cname}_DECLS_H\n"
    append sdecls "\#define ${cname}_DECLS_H\n"
    append sdecls "\n"
    append sdecls "\#include <tcl.h>\n"
    return
}

proc ::critcl::api::StubDeclTrailer {sv cname} {
    debug.critcl/api {}
    upvar 1 $sv sdecls
    # Standard footer for a *Decls.h header file.
    append sdecls "\#endif /* ${cname}_DECLS_H */\n"
    return
}

proc ::critcl::api::StubDeclExternals {sv context} {
    debug.critcl/api {}
    variable ehdrs
    if {![dict exists $ehdrs $context]} return
    upvar 1 $sv sdecls

    # Make all declared external header files accessible in *Decls.h

    append sdecls "\n"
    foreach hdr [dict get $ehdrs $context] {
	append sdecls "\#include \"[file tail $hdr]\"\n"
    }
    return
}

proc ::critcl::api::StubDeclPublic {sv context cname} {
    debug.critcl/api {}
    variable hdrs
    if {![dict exists $hdrs $context]} return
    upvar 1 $sv sdecls

    # Make all declared public header files of the package accessible
    # in *Decls.h. Place them in the result cache as well, for pick up
    # during compilation.

    append sdecls "\n"
    foreach hdr [dict get $hdrs $context] {
	set hfile [file tail $hdr]
	cache copy2 $hdr $cname/$hfile
	append sdecls "\#include \"$hfile\"\n"
    }
    return
}

proc ::critcl::api::StubDeclDef {sv context cname ename tv} {
    debug.critcl/api {}
    upvar 1 $sv sdecls $tv T

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

    variable scspec
    if {[dict exists $scspec $context]} {
	stubs::container::scspec T [dict get $scspec $context]
    }

    variable fun
    if {[dict exists $fun $context]} {
	set index 0
	foreach decl [dict get $fun $context] {
	    #puts D==|$decl|
	    stubs::container::declare T $cname $index generic $decl
	    incr index
	}
	append sdecls "\n"
	append sdecls [stubs::gen::header::gen $T $cname]
    }

    return
}

proc ::critcl::api::StubDeclStorage {sv cname upname} {
    debug.critcl/api {}
    upvar 1 $sv sdecls

    # Insert code to handle the storage class settings on Windows.

    lappend map @cname@ $cname
    lappend map @up@    $upname

    append sdecls [string map $map {
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
    return
}

proc ::critcl::api::StubUse {name desc cv cpv uv} {
    debug.critcl/api {}
    upvar 1 $cv cname $cpv capname $uv upname

    set cname   [string map {:: _} $iname]
    set upname  [string toupper  $cname]
    set capname [stubs::gen::cap $cname]

    return [at::here!][subst -nocommands {
	/* $desc */
	#define USE_${upname}_STUBS 1
	#include <$cname/${cname}Decls.h>
    }]
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::api {
    # Per-file (context) databases of stubs table information.

    # (1) Exported definitions of this package.
    variable scspec {} ;# dict (<context> -> sc-spec)
    variable self   {} ;# dict (<context> -> name)
    variable hdrs   {} ;# dict (<context> -> list (path))
    variable ehdrs  {} ;# dict (<context> -> list (path))
    variable fun    {} ;# dict (<context> -> list (decl))

    # self  - String. Name of our API. Defaults to package name.
    # hdrs  - List. Exported public headers of the API.
    # ehdrs - List. Exported external public headers of the API.
    # fun   - List. Exported functions (signatures of result type, name, and arguments (C syntax))

    # (2) Imported tables.
    variable use    {} ;# dict (<context> -> list-of (pair (pkgname pkgver)))

    namespace import ::critcl::cache
    namespace import ::critcl::cdefs
    namespace import ::critcl::common
    namespace import ::critcl::meta
    namespace import ::critcl::uuid
    namespace import ::critcl::tags
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::::api::LocateDecls {context name} {
    debug.critcl/api {}
    foreach dir [cdefs system-include-paths $context] {
	if {[DeclsAt $dir $name]} { return $dir }
    }
    return {}
}

proc ::critcl::::api::DeclsAt {dir name} {
    debug.critcl/api {}
    foreach suffix {
	Decls.h StubLib.h
    } {
	if {![file exists [file join $dir $name $name$suffix]]} { return 0 }
    }
    return 1
}

proc ::critcl::api::Error {msg args} {
    debug.critcl/api {}
    set code [linsert $args 0 CRITCL API]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
