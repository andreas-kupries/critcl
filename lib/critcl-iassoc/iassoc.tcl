## -*- tcl -*-
# # ## ### ##### ######## ############# #####################
# Pragmas for MetaData Scanner.
# n/a

# CriTcl Utility Commands. Specification of a C function and structure
# associated with an interpreter made easy.

package provide critcl::iassoc 1

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl    8.4   ; # Min supported version.
package require critcl 3.1   ; # Need 'meta?' to get the package name.
package require critcl::util ; # Use the package's Get/Put commands.

namespace eval ::critcl::iassoc {}

# # ## ### ##### ######## ############# #####################
## API: Generate the declaration and implementation files for the iassoc.

proc ::critcl::iassoc::def {name arguments struct constructor destructor} {
    # Arguments:
    # - name of the C function which will provide access to the
    #   structure. This name, with a fixed prefix is also used to
    #   identify the association within the interpreter, and for
    #   the structure's type.
    #
    # - C code declaring the structure's contents.
    # - C code executed to initialize the structure.
    # - C code executed to destroy the structure.

    # Note that this is, essentially, a singleton object, without
    # methods.

    # Pull the package we are working on out of the system.

    set package [critcl::meta? name]

    #puts "%%% Pkg  |$package|"
    #puts "%%% Name |$name|"

    #puts "@@@ <<$data>>"

    set stem  ${package}_iassoc_${name}
    set type  ${name}_data
    set label critcl::iassoc/p=$package/a=$name

    set anames {}
    if {[llength $arguments]} {
	foreach {t v} $arguments {
	    lappend alist "$t $v"
	    lappend anames $v
	}
	set arguments ", [join $alist {, }]"
	set anames ", [join $anames {, }]"
    }

    lappend map @package@     $package
    lappend map @name@        $name
    lappend map @stem@        $stem
    lappend map @label@       $label
    lappend map @type@        $type
    lappend map @struct@      $struct
    lappend map @argdecls@    $arguments
    lappend map @argnames@    $anames
    lappend map @constructor@ $constructor
    lappend map @destructor@  $destructor
    lappend map @destructor@  $destructor

    set hdr      ${stem}.h
    set header   [file join [critcl::cache] $hdr]
    set template [Template iassoc.h]

    #puts T=[string length $template]

    file mkdir [critcl::cache]
    critcl::util::Put $header [string map $map $template]

    critcl::ccode "#include <$hdr>"
    return
}

proc ::critcl::iassoc::Template {path} {
    variable selfdir
    set path $selfdir/$path
    #puts T=$path
    return [critcl::util::Get $path]
}

# # ## ### ##### ######## ############# #####################
##
# Internal: Namespace holding the specification commands and related
# state. Treat like a sub-package, with a proper API.
##
# # ## ### ##### ######## ############# #####################

namespace eval ::critcl::iassoc::spec {}

# # ## ### ##### ######## ############# #####################

# # ## ### ##### ######## ############# #####################
## State

namespace eval ::critcl::iassoc {
    variable selfdir [file dirname [file normalize [info script]]]
}

# # ## ### ##### ######## ############# #####################
## Export API

namespace eval ::critcl::iassoc {
    namespace export def
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## Ready
return
