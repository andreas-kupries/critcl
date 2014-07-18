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

package require Tcl 8.4            ;# Minimal supported Tcl runtime.
package require dict84             ;# Forward-compatible dict command.
package require lassign84          ;# Forward-compatible lassign command.
package require critcl::cache      ;# Access to result cache
package require critcl::common     ;# General critcl utilities.
package require critcl::meta       ;# Management of teapot meta data.
package require critcl::uuid       ;# Digesting, change detection.

# XXX SystemIncludePaths - cdefs!

package provide  critcl::api 1
namespace eval ::critcl::api {
    namespace export c_scspec c_import c_export \
	c_header c_extheader c_function \
	complete
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::api::c_scspec {file scspec} {
    variable scspec

    uuid::add $file .api-scspec $scspec
    dict set scspec $file $scspec
    return
}

proc ::critcl::api::c_import {file name version} {
    variable use

    # First we request the imported package, giving it a chance to
    # generate the headers searched for in a moment (maybe it was
    # critcl based as well, and generates things dynamically).

    # Note that this can fail, for example in a cross-compilation
    # environment. Such a failure however does not imply that the
    # required API headers are not present, so we can continue.

    catch {
	package require $name $version
    }

    meta::require $file [list $name $version]

    # Now we check that the relevant headers of the imported package
    # can be found in the specified search paths.

    set cname [string map {:: _} $name]

    set at [LocateDecls $file $cname]
    if {$at eq {}} {
	::critcl::error "Headers for API $name not found"
    } else {
	::critcl::msg -nonewline " (stubs import $name $version @ $at/$cname)"
    }

    set def [list $name $version]
    uuid::add $file .api-import $def
    dict lappend use $file $def

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

proc ::critcl::api::c_export {file name} {
    variable self
    ::critcl::msg -nonewline " (stubs export $name)"

    uuid::add $file .api-self $name
    dict set self $file $name
    return $name
}

proc ::critcl::api::c_header {file args} {
    variable hdrs

    set base [file dirname $file]

    uuid::add $file .api-headers $args

    # XXX FIXME / InitializeFile
    # XXX FIXME can we use the uuid info as the flag showing init?

    foreach pattern $args {
	foreach v [common::expand-glob $base $pattern] {
	    dict lappend hdrs $file $v
	}
    }
    return
}

proc ::critcl::api::c_extheader {file args} {
    variable ehdrs

    uuid::add $file .api-eheaders $args

    # XXX FIXME / InitializeFile
    # XXX FIXME can we use the uuid info as the flag showing init?

    foreach v $args {
	dict lappend ehdrs $file $v
    }
    return
}

proc ::critcl::api::c_function {file rtype name arguments} {
    variable fun

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
    dict lappend fun $file $decl
    return
}

proc ::critcl::api::clear {ref} {
    variable scspec ; dict unset scspec $ref
    variable self   ; dict unset self   $ref
    variable hdrs   ; dict unset hdrs   $ref
    variable ehdrs  ; dict unset ehdrs  $ref
    variable fun    ; dict unset fun    $ref
    variable use    ; dict unset use    $ref
    return
}

proc ::critcl::api::complete {ref} {
    # Generate various pieces of C code needed to import the used
    # stubs tables into the package.

    set flags   {} ;# list: cc flags to build with stubs.
    set defines {} ;# list: C defines activating use of imported stubs.
    set code    {} ;# list: C fragments for including stubs decl headers.
    set decls   {} ;# list: C fragments for including stubs lib headers.
                    #       (in lieu of a libXXXstub.a).
    set init    {} ;# list: C fragments for calling stubs init functions

    CompleteImport $ref
    set cname [CompleteExport $ref]

    return [list $cname $defines $flags $code $decls $init]
}

proc ::critcl::api::CompleteImport {ref} {
    variable use
    if {![dict exists $use $ref]} return

    upvar 1 defines defines code code decls decls init init

    #::critcl::msg -nonewline " (stubs import completion)"

    package require stubs::gen

    foreach def [dict get $use $ref] {
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

proc ::critcl::api::CompleteExport {ref} {
    variable hdrs
    variable ehdrs
    variable fun

    if {![dict exists $hdrs  $ref] &&
	![dict exists $ehdrs $ref] &&
	![dict exists $fun   $ref]} return

    #::critcl::msg -nonewline " (stubs export completion)"

    variable self
    upvar 1 flags flags code code decls decls init init

    if {[dict exists $self $ref]} {
	# API name was declared explicitly
	set ename [dict get $self $ref]
    } else {
	# API name is implicitly defined, as the package name.
	set ename [meta::gets $ref name]
    }

    lappend code [StubUse $ename \
		      "Import our own exported API: $ename, mapping disabled" \
		      cname capname upname]

    StubDeclHeader    sdecls      $cname
    StubDeclExternals sdecls $ref
    StubDeclPublic    sdecls $ref $cname
    StubDeclStorage   sdecls      $cname $upname
    StubDeclDef       sdecls $ref $cname $ename  T
    StubDeclTrailer   sdecls $cname

    cache::write $cname/${cname}Decls.h   $sdecls

    set comment "/* Stubs API Export: $ename */"

    lappend init  [StubDeclInitCode $ref $cname $comment]
    lappend decls [StubDeclInitDecl $T $comment]
    lappend flags -DBUILD_$cname

    # Save the header files to the result cache for pickup (importers
    # in mode "compile & run", or by the higher-level code doing a
    # "generate package")

    cache::write $cname/${cname}.decls    [stubs::writer::gen   $T]
    cache::write $cname/${cname}StubLib.h [stubs::gen::lib::gen $T]

    return $cname
}

proc ::critcl::api::StubDeclInitDecl {T comment} {
    set    sinitstatic "  $comment\n  "
    append sinitstatic [stubs::gen::init::gen $T]

    return $sinitstatic
}

proc ::critcl::api::StubDeclInitCode {ref cname comment} {
    set pn [meta::gets $ref name]
    set pv [meta::gets $ref version]

    append sinitrun $comment
    append sinitrun \n
    append sinitrun "Tcl_PkgProvideEx (ip, \"$pn\", \"$pv\", (ClientData) &${cname}Stubs);"

    return $sinitrun
}

proc ::critcl::api::StubDeclHeader {sv cname} {
    upvar 1 $sv sdecls
    # Standard heading for a *Decls.h header file.

    append sdecls "\#ifndef ${cname}_DECLS_H\n"
    append sdecls "\#define ${cname}_DECLS_H\n"
    append sdecls "\n"
    append sdecls "\#include <tcl.h>\n"
    return
}

proc ::critcl::api::StubDeclTrailer {sv cname} {
    upvar 1 $sv sdecls
    # Standard footer for a *Decls.h header file.
    append sdecls "\#endif /* ${cname}_DECLS_H */\n"
    return
}

proc ::critcl::api::StubDeclExternals {sv ref} {
    variable ehdrs
    if {![dict exists $ehdrs $ref]} return
    upvar 1 $sv sdecls

    # Make all declared external header files accessible in *Decls.h

    append sdecls "\n"
    foreach hdr [dict get $ehdrs $ref] {
	append sdecls "\#include \"[file tail $hdr]\"\n"
    }
    return
}

proc ::critcl::api::StubDeclPublic {sv ref cname} {
    variable hdrs
    if {![dict exists $hdrs $ref]} return
    upvar 1 $sv sdecls

    # Make all declared public header files of the package accessible
    # in *Decls.h. Place them in the result cache as well, for pick up
    # during compilation.

    append sdecls "\n"
    foreach hdr [dict get $hdrs $ref] {
	set hfile [file tail $hdr]
	cache::copy2 $hdr $cname/$hfile
	append sdecls "\#include \"$hfile\"\n"
    }
    return
}

proc ::critcl::api::StubDeclDef {sv ref cname ename tv} {
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
    if {[dict exists $scspec $ref]} {
	stubs::container::scspec T [dict get $scspec $ref]
    }

    variable fun
    if {[dict exists $fun $ref]} {
	set index 0
	foreach decl [dict get $fun $ref] {
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
    # Per-file (ref) databases of stubs table information.

    # (1) Exported definitions of this package.
    variable scspec {} ;# dict (<ref> -> sc-spec)
    variable self   {} ;# dict (<ref> -> name)
    variable hdrs   {} ;# dict (<ref> -> list (path))
    variable ehdrs  {} ;# dict (<ref> -> list (path))
    variable fun    {} ;# dict (<ref> -> list (decl))

    # self  - String. Name of our API. Defaults to package name.
    # hdrs  - List. Exported public headers of the API.
    # ehdrs - List. Exported external public headers of the API.
    # fun   - List. Exported functions (signatures of result type, name, and arguments (C syntax))

    # (2) Imported tables.
    variable use    {} ;# dict (<ref> -> list-of (pair (pkgname pkgver)))

    namespace eval cache  { namespace import ::critcl::cache::*  }
    namespace eval common { namespace import ::critcl::common::* }
    namespace eval meta   { namespace import ::critcl::meta::*   }
    namespace eval uuid   { namespace import ::critcl::uuid::*   }
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::::api::LocateDecls {ref name} {
    # XXX FIXME back reference - backend - include paths ...
    # XXX FIXME actually based on global options
    foreach dir [SystemIncludePaths $ref] {
	if {[DeclsAt $dir $name]} { return $dir }
    }
    return {}
}

proc ::critcl::::api::DeclsAt {dir name} {
    foreach suffix {
	Decls.h StubLib.h
    } {
	if {![file exists [file join $dir $name $name$suffix]]} { return 0 }
    }
    return 1
}

proc ::critcl::api::Error {msg args} {
    set code [linsert $args 0 CRITCL API]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
