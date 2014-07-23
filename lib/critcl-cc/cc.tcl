# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# This package implements the backend of the system, calling on the C
# compiler, linker, and other external commands to build, link, and
# load the accumulated C code.

# TODO: In a second step this will be renamed to something like
# 'critcl::cc::external' and 'critcl::cc' then becomes a shim
# responsible for (a) detection of the available backends, and
# (b) selecting and loading a backend.

# XXX FIXME: This package should be the only one accessing the CC
# XXX FIXME: configuration database, i.e. "critcl::ccconfig".

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4           ;# Minimal supported Tcl runtime.
package require dict84            ;# Forward compatible dict command.
package require lassign84         ;# Forward compatible lassign command.
package require critcl::cache     ;# Result cache access.
package require critcl::ccconfig  ;# CC configuration database for standard backend.
package require critcl::cdefs     ;# General collection of C definitions.
package require critcl::common    ;# General utility commands.
package require critcl::data      ;# Access to templates and other supporting files.
package require critcl::gopt      ;# Management of global options.
package require critcl::log       ;# Log files within the result cache.
package require critcl::meta      ;# Management of teapot meta data.
package require critcl::tags      ;# Management of indicator flags.
package require critcl::uuid      ;# UUID generation.

package provide  critcl::cc 1
namespace eval ::critcl::cc {
    namespace export has-compiler build-direct link-direct \
	build-for
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::cc::has-compiler {} {
    return [llength [auto_execok [lindex [config::get compile] 0]]]
}

proc ::critcl::cc::build-direct {ref label code {mode temp}} {
    # ref <=> context = .critcl file under whose aegis this is done.
    # => Provides access to custom flags, paths, etc.

    set src [cache::write check_[pid].c $code]
    set obj [file rootname $src][config::get object]

    # See also the internal helper command 'Compile'. The code here is
    # in essence a simplified form of that.

    set         cmdline [config::get compile]
    DebugFlags  cmdline $ref
    lappendlist cmdline [cdefs::flags? $ref]
    lappendlist cmdline [IncludesOf [cdefs::system-include-paths $ref]]
    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $src

    log::begin $v::prefix $ref
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

proc ::critcl::cc::link-direct {ref label code} {
    # ref <=> context = .critcl file under whose aegis this is done.
    # => Provides access to custom flags, paths, etc.

    set ok [build-direct $ref "$label (build)" $code keep]
    if {!$ok} {
	return 0
    }

    set out [cache::get check_[pid].out]
    set obj [file rootname $out][config::get object]

    set cmdline [config::get link]

    if {[tags::has $ref debug-symbols]} {
	lappendlist cmdline [config::get link_debug]
    } else {
	lappendlist cmdline [config::get strip]
	lappendlist cmdline [config::get link_release]
    }

    lappendlist cmdline [LinkResult $out]
    lappendlist cmdline $obj
    lappendlist cmdline [LibrariesOf [cdefs::system-lib-paths $ref]]
    # XXX NOTE system-lib-paths used inside of Fix ...
    # XXX NOTE could make use of clibrary information, i.e. same.
    lappendlist cmdline [FixLibraries $ref [cdefs::libs? $ref]]
    lappendlist cmdline [cdefs::ldflags? $ref]

    log::text "${label} (link)... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK ERR]

    log::done
    cache::clear check_[pid].*
    return $ok
}

proc ::critcl::cc::build-for {rv prefix file buildforpackage load} {
    upvar 1 $rv result

    StatusReset

    # Determine if we should place stubs code into the generated file.
    set placestubs [expr {!$buildforpackage}]

    # NOTE: The 4 pieces of data just below are always required, even
    # if the build and link-steps are suppressed. The load-step must
    # have this information.

    # Basename for all generated files (.c, .o, .so)
    set base     [file normalize [cache::get ${prefix}_[uuid::get $file]]]
    set shlib    [DetermineShlibName $base]
    set initname [DetermineInitName  $file $buildforpackage]
    set tsources [cdefs::tcls?       $file]
    set mintcl   [cdefs::usetcl?     $file]

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
	log::begin $prefix $file

	# XXX apiprefix header.c vs [ccode] (see (**)) ?! critcl.tcl
	if {[dict exists $result apiprefix]} {
	    set api [dict get $result apiprefix]
	} else {
	    set api ""
	}

	# Generate the main C file
	# XXX FIXME - This should be put into cdefs...
	# XXX FIXME different API re file name/path/fd ?

	# XXX FIXME split out BuildDefines...

	CollectEmbeddedSources $file $api $base.c $initname $placestubs $mintcl

	# Set the marker for "critcl::done" and "CheckEntry".
	tags::set $file done

	set object [DetermineObjectName $base $file]
	dict set result object $object

	# Compile main file
        lappend objects [Compile result $file $file $base.c $object]

	# Compile the companion C sources as well, if there are any.
        foreach src [cdefs::srcs? $file] {
	    lappend objects [Compile result $file $src $src [CompanionObject $src]]
	}

	# NOTE: The information below has to be copied into the result
	# even if the link-step is suppressed. Because the application
	# (mode 'generate package') must know all this to be able to
	# perform the final link.

	lappendlist objects [cdefs::objs? $file]

	set ldflags [cdefs::ldflags? $file]
	set preload [cdefs::preload? $file]

	dict set result clibraries [cdefs::libs? $file]
	dict set result ldflags    $ldflags
	dict set result objects    $objects
	dict set result tk         [cdefs::usetk? $file]
	dict set result preload    $preload
	####	dict set result license    [GetParam $file license <<Undefined>>] ;# XXX FIXME license handling
	dict set result log        {}
	dict set result meta       [meta::getall $file]

	# Link and load steps.
        if {$load || !$buildforpackage} {
	    Link result $file $shlib $preload $ldflags
	}

	set msgs [log::done]
	dict set result warnings [CheckForWarnings $msgs]
    }

    if {[Failed]} {
	if {!$buildforpackage} {
	    # XXX FIXME move into caller? => branch here is c&run or bracket code.
	    print stderr "$msgs\ncritcl build failed ($file)"
	} else {
	    dict set result log $msgs
	}
    } elseif {$load && !$buildforpackage} {
	Load $shlib $initname $tsources
    }

    # Save final status
    tags::set $file failed [Failed]
    StatusReset
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::cc {
    # Status flag used by compile, link, etc. operations.
    variable failed {}

    namespace eval cache  { namespace import ::critcl::cache::*    }
    namespace eval cdefs  { namespace import ::critcl::cdefs::*    }
    namespace eval config { namespace import ::critcl::ccconfig::* }
    namespace eval data   { namespace import ::critcl::data::*     }
    namespace eval gopt   { namespace import ::critcl::gopt::*     }
    namespace eval log    { namespace import ::critcl::log::*      }
    namespace eval meta   { namespace import ::critcl::meta::*     }
    namespace eval tags   { namespace import ::critcl::tags::*     }
    namespace eval uuid   { namespace import ::critcl::uuid::*     }
    namespace import ::critcl::common::lappendlist
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::cc::DetermineShlibName {base} {
    # The name of the shared library we hope to produce (or use)
    return ${base}[config::sharedlibext]
}

proc ::critcl::cc::DetermineInitName {file buildforpackage} {
    set ininame [PkgInit $file]

    # Add in the build prefix, if specified. This is done in mode
    # 'generate package', for the pieces, ensuring that the overall
    # initialization function cannot be in conflict with the
    # initialization functions of these same pieces.

    if {$buildforpackage} {
        set ininame "ns_$ininame"
    }

    return $ininame
}

proc ::critcl::cc::PkgInit {file} {
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

proc ::critcl::cc::DetermineObjectName {base file} {
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
    if {[tags::has $file debug-symbols]} {
        append object _g
    }

    # Choose a distinct suffix so switching between them causes a
    # rebuild.
    switch -- [gopt::get combine] {
	""         -
	dynamic    { append object _pic  [config::get object] }
	static     { append object _stub [config::get object] }
	standalone { append object       [config::get object] }
    }

    return $object
}

proc ::critcl::cc::Compile {rv tclfile origin cfile obj} {
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

    set         cmdline [config::get compile]
    lappendlist cmdline [cdefs::flags? $tclfile]
    DebugFlags  cmdline $file
    lappendlist cmdline [config::get threadflags]
    if {[gopt::get combine] ne "standalone"} {
	lappendlist cmdline [config::get tclstubs]
    }
    if {[gopt::get language] ne "" && [file tail $tclfile] ne "critcl.tcl"} {
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	# See also -x none below.
	lappend cmdline -x [gopt::get language]
    }
    lappendlist cmdline [IncludesOf [TclIncludes [cdefs::usetcl? $tclfile]]]
    lappendlist cmdline [IncludesOf [cdefs::system-include-paths $tclfile]]

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
    if {[cdefs::usetk? $tclfile] &&
	([gopt::get combine] ne "standalone")} {
	lappendlist cmdline [config::get tkstubs]
    }

    if {![tags::has $tclfile debug-symbols]} {
	lappendlist cmdline [config::get optimize]
	lappendlist cmdline [config::get noassert]
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

proc ::critcl::cc::Link {rv file shlib preload ldflags} {
    upvar 1 $rv result

    # See also link-direct for possible code sharing...

    StatusAbort?

    # Assemble the link command.
    set cmdline [config::get link]

    if {[llength $preload]} {
	lappendlist cmdline [config::get link_preload]
    }

    if {[tags::has $file debug-symbols]} {
	lappendlist cmdline [config::get link_debug]
    } else {
	lappendlist cmdline [config::get strip]
	lappendlist cmdline [config::get link_release]
    }

    lappendlist cmdline [LinkResult $shlib]
    lappendlist cmdline [GetObjects $result]
    lappendlist cmdline [LibrariesOf [cdefs::system-lib-paths $file]]

    # XXX NOTE clibraries <=> [cdefs::libs?]
    # XXX NOTE system-lib-paths used inside of Fix ...
    # XXX NOTE could make use of clibrary information, i.e. same.
    lappendlist cmdline [FixLibraries $file [dict get $result clibraries]]
    lappendlist cmdline $ldflags
    # lappend cmdline bufferoverflowU.lib
    #      msvc >=1400 && <1500 for amd64
    # Handled in the configuration.

    # Run the linker
    ExecWithLogging $cmdline \
	{$shlib: [file size $shlib] bytes} \
	{ERROR while linking $shlib:}

    # Now, if there is a manifest file around, and the
    # 'embed_manifest' command defined we use its command to merge the
    # manifest into the shared library. This is pretty much only
    # happening on Windows platforms, and with newer dev environments
    # actually using manifests.

    set em [config::get embed_manifest]

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
	::critcl::Ignore $t
	source $t
    }
    return
}

proc ::critcl::cc::DebugFlags {cv file} {
    upvar 1 $cv cmdline
    if {[tags::has $file debug-symbols]} {
	lappendlist cmdline [config::get debug_symbols]
    }
    if {[tags::has $file debug-memory]} {
	lappendlist cmdline [config::get debug_memory]
    }
    return
}

proc ::critcl::cc::TclIncludes {tclversion} {
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

    return [list $path]
}

proc ::critcl::cc::GetObjects {result} {
    # On windows using the native MSVC compiler put the companion
    # object files into a link file to read, instead of separately on
    # the command line.

    # XXX FIXME - Use cdefs directly?

    set objects [dict get $result objects]

    if {![string match "win32-*-cl" [config::buildplatform]]} {
	return $objects
    }

    set rsp [cache::write link.fil \"[join $objects \"\n\"]\"]
    return [list @$rsp]
}

proc ::critcl::cc::MakePreloadLibrary {rv file} {
    upvar 1 $rv result

    StatusAbort?

    # compile and link the preload support, if necessary, i.e. not yet
    # done.

    set shlib [cache::get preload[config::sharedlibext]]
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
    set         cmdline [config::get link]
    lappend     cmdline $obj
    lappendlist cmdline [config::get strip]
    lappendlist cmdline [LinkResult $shlib]

    ExecWithLogging $cmdline \
	{$shlib: [file size $shlib] bytes} \
	{ERROR while linking $shlib:}

    # Now the critcl application can pick up this helper shlib and
    # stuff it into the package it is making.
    return
}

proc ::critcl::cc::ManifestCommand {em shlib} {
    # Variable used by the subst'able config setting.
    set outfile $shlib
    return [subst $em]
}

proc ::critcl::cc::CompanionObject {src} {
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

    return [cache::get ${srcbase}[config::get object]]

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

proc ::critcl::cc::GetLibraries {file result} {
    # On windows using the native MSVC compiler, transform all -lFOO
    # references into FOO.lib.
    return [FixLibraries $file [dict get $result clibraries]]
}

proc ::critcl::cc::FixLibraries {file libraries} {
    if {[string match "win32-*-cl" [config::buildplatform]]} {
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
	set lpath [cdefs::system-lib-paths $file]

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

proc ::critcl::cc::ResolveColonSpec {lpath name} {
    foreach path $lpath {
	set f [file join $lpath $name]
	if {![file exists $f]} continue
	return $f
    }
    return -l:$name
}

proc ::critcl::cc::CompileResult {object} {
    # Variable used by the subst'able config setting.
    set outfile $object
    return [subst [config::get output]]
}

proc ::critcl::cc::LinkResult {shlib} {
    # Variable used by the subst'able config setting.
    set outfile $shlib

    set ldout [subst [config::get ldoutput]]
    if {$ldout eq ""} {
	set ldout [subst [config::get output]]
    }

    return $ldout
}

proc ::critcl::LibrariesOf {directories} {
    set flag [config::get libinclude]
    set libincludes {}
    foreach dir $directories {
	lappend libincludes $flag$dir
    }
    return $libincludes
}

proc ::critcl::cc::IncludesOf {directories} {
    set flag [config::get include]
    set includes {}
    foreach dir $directories {
	lappend includes $flag$dir
    }
    return $includes
}

# # ## ### ##### ######## ############# #####################
## Status management, and actual execution.

proc ::critcl::cc::Failed {} {
    variable failed
    return  $failed
}

proc ::critcl::cc::StatusReset {} {
    variable failed 0
    return
}

proc ::critcl::cc:StatusAbort? {} {
    variable failed
    if {$failed} { return -code return }
    return
}

proc ::critcl::cc::CheckForWarnings {text} {
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

proc ::critcl::cc::ExecWithLogging {cmdline okmsg errmsg} {
    variable failed

    set w [join [lassign $cmdline cmd] \n\t]
    log::text \n$cmd\n\t$w\n

    set ok [config::do-log failed err [log::fd] $cmdline]

    if {$ok} {
	log::line [uplevel 1 [list subst $okmsg]]
    } else {
	log::line [uplevel 1 [list subst $errmsg]]
	log::line $err
    }

    return $ok
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
