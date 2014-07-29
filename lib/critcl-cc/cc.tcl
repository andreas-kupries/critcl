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

package require Tcl 8.4             ;# Minimal supported Tcl runtime.
package require critcl::cache       ;# Result cache access.
package require critcl::cc::config  ;# CC configuration database for standard backend.
package require critcl::cdefs       ;# General collection of C definitions.
package require critcl::common      ;# General utility commands.
package require critcl::data        ;# Access to templates and other supporting files.
package require critcl::gopt        ;# Management of global options.
package require critcl::log         ;# Log files within the result cache.
package require critcl::tags        ;# Management of indicator flags.
package require debug               ;# debug narrative

package provide critcl::cc 4

namespace eval ::critcl::cc {
    namespace export has-compiler extract-const build-direct link-direct \
	build-immediate-start build-immediate-required build-immediate \
	build-immediate-complete
    namespace ensemble create
}

debug level  critcl/cc
debug prefix critcl/cc {[debug caller] | }

# # ## ### ##### ######## ############# #####################
##
# This allows a backend to provide fake information.
# I.e a tcc4tcl backend is compile&run only, no x-compile, no configs, etc ...

interp alias {} ::critcl::cc::read           {} ::critcl::cc::config::read
interp alias {} ::critcl::cc::use            {} ::critcl::cc::config::use
interp alias {} ::critcl::cc::showall        {} ::critcl::cc::config::showall
interp alias {} ::critcl::cc::show           {} ::critcl::cc::config::show
interp alias {} ::critcl::cc::get            {} ::critcl::cc::config::get
interp alias {} ::critcl::cc::known          {} ::critcl::cc::config::known
interp alias {} ::critcl::cc::target         {} ::critcl::cc::config::target
interp alias {} ::critcl::cc::targetplatform {} ::critcl::cc::config::targetplatform
interp alias {} ::critcl::cc::buildplatform  {} ::critcl::cc::config::buildplatform
interp alias {} ::critcl::cc::actual         {} ::critcl::cc::config::actual
interp alias {} ::critcl::cc::sharedlibext   {} ::critcl::cc::config::sharedlibext
interp alias {} ::critcl::cc::crosscheck     {} ::critcl::cc::config::crosscheck    

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::cc::has-compiler {} {
    debug.critcl/cc {}
    return [llength [auto_execok [lindex [config::get compile] 0]]]
}

proc ::critcl::cc::extract-const {context} {
    debug.critcl/cc {}
    # result = list (label value ...)
    # Elements are all found defines and enum symbols.
    # This includes platform- and cc-specific defines.

    set defines {}
    # we process the cdefines in three steps
    #   - get the list of defines by preprocessing the source using the
    #     cpp -dM directive which causes any #defines to be output
    #   - extract the list of enums using regular expressions (not perfect,
    #     but will do for now)
    #   - generate Tcl_ObjSetVar2 commands to initialise Tcl variables

    # Pull the collected ccode blocks together into a transient file
    # we then search in.

    set def     define_[pid].c
    set dcode   [cdefs code? $context defs]
    set defpath [cache write $def $dcode]
    set hdrs    [IncludesOf [cdefs system-include-paths $context]]

    # # ## ### ##### ######## ############# #####################
    # # ## ### ##### ######## ############# #####################
    # For the command lines to be constructed we need all the include
    # information the regular files will get during their compilation.

    # First step - get a list of defines

    set         cmd [config::get preproc_define]
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

	lappend defines $var $val
    }
    close $pipe

    # Second step - get list of enums

    set         cmd [config::get preproc_enum]
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

		lappend defines $enum $enum
	    }
	}
    }

    # Cleanup after ourselves, removing the helper file.
    if {![gopt get keepsrc]} {
	cache clear $def
    }

    return $defines
}

proc ::critcl::cc::build-direct {context label code {mode temp}} {
    debug.critcl/cc {}
    # context = .critcl file under whose aegis this is done.
    # => Provides access to custom flags, paths, etc.

    set src [cache write check_[pid].c $code]
    set obj [file rootname $src][config::get object]

    # See also the internal helper command 'Compile'. The code here is
    # in essence a simplified form of that.

    set         cmdline [config::get compile]
    DebugFlags  cmdline $context
    lappendlist cmdline [cdefs flags? $context]
    lappendlist cmdline [IncludesOf [cdefs system-include-paths $context]]
    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $src

    log begin $v::prefix $context
    log text  "${label}... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK FAILED]
    StatusReset

    if {!$ok || ($mode eq "temp")} {
	log done
	cache clear check_[pid].*
    }
    return $ok
}

proc ::critcl::cc::link-direct {context label code} {
    debug.critcl/cc {}
    # context = .critcl file under whose aegis this is done.
    # => Provides access to custom flags, paths, etc.

    set ok [build-direct $context "$label (build)" $code keep]
    if {!$ok} {
	return 0
    }

    set out [cache get check_[pid].out]
    set obj [file rootname $out][config::get object]

    set cmdline [config::get link]

    if {[tags has $context debug-symbols]} {
	lappendlist cmdline [config::get link_debug]
    } else {
	lappendlist cmdline [config::get strip]
	lappendlist cmdline [config::get link_release]
    }

    lappendlist cmdline [LinkResult $out]
    lappendlist cmdline $obj
    lappendlist cmdline [LibrariesOf [cdefs system-lib-paths $context]]
    # XXX NOTE system-lib-paths used inside of Fix ...
    # XXX NOTE could make use of clibrary information, i.e. same.
    lappendlist cmdline [GetLibraries $context]
    lappendlist cmdline [cdefs ldflags? $context]

    log text "${label} (link)... "
    StatusReset
    set ok [ExecWithLogging $cmdline OK ERR]

    log done
    cache clear check_[pid].*
    return $ok
}

proc ::critcl::cc::build-immediate-begin {sv context prefix base initname} {
    debug.critcl/cc {}
    upvar 1 $sv ccstate

    # The state variable can be used by the members of the
    # build-immediate-* command-set as they see fit. The caller cannot
    # assume anything about it.

    set shlib [DetermineShlibName $base]

    # The ccstate dictionary is local information.

    #	initname - String. Foo in Foo_Init().
    #	shlib	 - String. Name of the shared library backing <context>.
    #	base	 - String. Common prefix (file root) of 'object' and 'shlib'.
    #   prefix   - String. General prefix for temp files.

    #--	license  - String. License text.

    dict set ccstate base     $base
    dict set ccstate initname $initname
    dict set ccstate prefix   $prefix
    dict set ccstate shlib    $shlib
    dict set ccstate context  $context

    StatusReset
    return
}

proc ::critcl::cc::build-immediate-required {sv} {
    debug.critcl/cc {}
    upvar 1 $sv ccstate

    if {[gopt get force]} { return yes }

    set shlib [dict get $ccstate shlib]
    if {![file exists $shlib]} { return yes }

    return no
}

proc ::critcl::cc::build-immediate {sv pkgcfile} {
    debug.critcl/cc {}
    upvar 1 $sv ccstate

    set prefix  [dict get $ccstate prefix]
    set context [dict get $ccstate context]
    set base    [dict get $ccstate base]

    log begin $prefix $context

    # XXX WHY apiprefix header.c vs [ccode]
    # XXX WHY (see (**)) ?! critcl.tcl

    set object [DetermineObjectName $base $context]

    # Compile main file
    Compile $context $context $pkgcfile $object
    cdefs objs $context [list $object]

    # Compile the companion C sources as well, if there are any.
    foreach src [cdefs srcs? $context] {
	set object [CompanionObject $src]
	Compile $context $src $src $object
	cdefs objs $context [list $object]
    }

    # XXX see if the cdefs can be moved inside.
    Link $context $shlib

    dict set ccstate msgs [log done]
    return
}

proc ::critcl::cc::build-immediate-complete {sv load} {
    debug.critcl/cc {}
    upvar 1 $sv ccstate

    set context [dict get $ccstate context]
    set failed  [Failed]

    if {$failed} {
	set msgs [dict get $ccstate msgs]
	# ATTENTION: This is in mode 'compile & run'.
	# How to report the trouble ?
	# We are already using the regular result (see "cbuild") to
	# report the general status (ok|failed). And if we were called
	# via [unknown] even that is swallowed.

	# XXX FIXME better error reporting on dynamic failure.
	# XXX FIXME maybe storage into tags like 'failed' ?
	# XXX FIXME then caller can decide how to report and what.

	print stderr "$msgs\ncritcl build failed ($context)"
    } elseif {$load} {
	set shlib    [dict get $ccstate shlib]
	set initname [dict get $ccstate initname]
	set tsources [cdefs tcls? $context]

	Load $shlib $initname $tsources
    }

    # Save final status
    tags set $context failed $failed
    StatusReset
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::cc {
    # Status flag used by compile, link, etc. operations.
    variable failed {}

    namespace import ::critcl::cache
    namespace import ::critcl::cdefs
    #namespace import ::critcl::cc::config - Implied child namespace
    namespace import ::critcl::data
    namespace import ::critcl::gopt
    namespace import ::critcl::log
    namespace import ::critcl::tags
    namespace import ::critcl::common::lappendlist
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::cc::DetermineShlibName {base} {
    debug.critcl/cc {}
    # The name of the shared library we hope to produce (or use)
    return ${base}[config::sharedlibext]
}

proc ::critcl::cc::DetermineObjectName {base context} {
    debug.critcl/cc {}
    set object $base

    # The generated object file will be saved for permanent use if the
    # outdir option is set (in which case rebuilds will no longer be
    # automatic).

    set odir [gopt get outdir]
    if {$odir ne ""} {
	set odir   [file join [file dirname $context] $odir]
	set oroot  [file rootname [file tail $context]]
	set object [file normalize [file join $odir $oroot]]
	file mkdir $odir
    }

    # Modify the output file name if debugging symbols are requested.
    if {[tags has $context debug-symbols]} {
        append object _g
    }

    # Choose a distinct suffix so switching between them causes a
    # rebuild.
    switch -- [gopt get combine] {
	""         -
	dynamic    { append object _pic  [config::get object] }
	static     { append object _stub [config::get object] }
	standalone { append object       [config::get object] }
    }

    return $object
}

proc ::critcl::cc::Compile {context origin cfile obj} {
    debug.critcl/cc {}
    StatusAbort?

    # context = The .tcl file under whose auspices the C is compiled.
    # origin  = The origin of the C sources, either context, or cfile.
    # cfile   = The file holding the C sources to compile.
    #
    # We have 'origin == cfile' for the companion C files of a critcl
    # file, i.e. the csources.
    #
    # For a .tcl critcl file, we have 'origin == context' instead, and
    # the cfile is the .c derived from context.
    #
    # obj = Object file to compile to, to generate.

    set         cmdline [config::get compile]
    lappendlist cmdline [cdefs flags? $context]
    DebugFlags  cmdline $file
    lappendlist cmdline [config::get threadflags]
    if {[gopt get combine] ne "standalone"} {
	lappendlist cmdline [config::get tclstubs]
    }
    if {[gopt get language] ne "" && [file tail $context] ne "critcl.tcl"} {
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	# See also -x none below.
	lappend cmdline -x [gopt get language]
    }
    lappendlist cmdline [IncludesOf [TclIncludes [cdefs usetcl? $context]]]
    lappendlist cmdline [IncludesOf [cdefs system-include-paths $context]]
    lappendlist cmdline [tags get $context apidefines]

    lappendlist cmdline [CompileResult $obj]
    lappend     cmdline $cfile

    if {[gopt get language] ne ""} {
	# Allow the compiler to determine the type of file otherwise
	# it will try to compile the libs
	# XXX Is this gcc specific ?
	# XXX Should this not be configurable via some c::* setting ?
	lappend cmdline -x none
    }

    # Add the Tk stubs to the command line, if requested and not suppressed
    if {[cdefs usetk? $context] &&
	([gopt get combine] ne "standalone")} {
	lappendlist cmdline [config::get tkstubs]
    }

    if {![tags has $context debug-symbols]} {
	lappendlist cmdline [config::get optimize]
	lappendlist cmdline [config::get noassert]
    }

    if {[ExecWithLogging $cmdline \
	     {$obj: [file size $obj] bytes} \
	     {ERROR while compiling code in $origin:}]} {
	if {![gopt get keepsrc] && $cfile ne $origin} {
	    file delete $cfile
	}
    }

    return $obj
}

proc ::critcl::cc::Link {context shlib} {
    debug.critcl/cc {}
    # See also link-direct for possible code sharing...

    StatusAbort?

    set preload [cdefs preload? $context]
    set ldflags [cdefs ldflags? $context]

    # Assemble the link command.
    set cmdline [config::get link]

    if {[llength $preload]} {
	lappendlist cmdline [config::get link_preload]
    }

    if {[tags has $context debug-symbols]} {
	lappendlist cmdline [config::get link_debug]
    } else {
	lappendlist cmdline [config::get strip]
	lappendlist cmdline [config::get link_release]
    }

    lappendlist cmdline [LinkResult $shlib]
    lappendlist cmdline [GetObjects $context]
    lappendlist cmdline [LibrariesOf [cdefs system-lib-paths $context]]

    # XXX NOTE clibraries <=> [cdefs libs?]
    # XXX NOTE system-lib-paths used inside of Fix ...
    # XXX NOTE could make use of clibrary information, i.e. same.
    lappendlist cmdline [GetLibraries $context]
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

    log line "Manifest Command: $em"
    log line "Manifest File:    [expr {[file exists $shlib.manifest]
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
	MakePreloadLibrary $context
    }
    return
}

proc ::critcl::Load {shlib init tsrc} {
    debug.critcl/cc {}
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
    debug.critcl/cc {}
    upvar 1 $cv cmdline
    if {[tags has $file debug-symbols]} {
	lappendlist cmdline [config::get debug_symbols]
    }
    if {[tags has $file debug-memory]} {
	lappendlist cmdline [config::get debug_memory]
    }
    return
}

proc ::critcl::cc::TclIncludes {tclversion} {
    debug.critcl/cc {}
    # Provide access to the Tcl/Tk headers using a -I flag pointing
    # into the critcl package directory hierarchy. No copying of files
    # required. This also handles the case of the X11 headers on
    # windows, for free.

    set path [data hdr tcl$tclversion]
    if {[file system $path] ne "native"} {
	# The critcl package is wrapped. Copy the relevant headers out
	# to disk (cache) and change the include path appropriately.
	set path [cache copy2 $path]
    }

    return [list $path]
}

proc ::critcl::cc::GetObjects {file} {
    debug.critcl/cc {}
    # On windows using the native MSVC compiler put the companion
    # object files into a link file to read, instead of separately on
    # the command line.

    set objects [cdefs objs? $file]

    if {![string match "win32-*-cl" [config::buildplatform]]} {
	return $objects
    }

    set rsp [cache write link.fil \"[join $objects \"\n\"]\"]
    return [list @$rsp]
}

proc ::critcl::cc::MakePreloadLibrary {file} {
    debug.critcl/cc {}
    StatusAbort?

    # compile and link the preload support, if necessary, i.e. not yet
    # done.

    set shlib [cache get preload[config::sharedlibext]]
    if {[file exists $shlib]} return

    # Operate like TclIncludes. Use the template file directly, if
    # possible, or, if we reside in a virtual filesystem, copy it to
    # disk.

    set src [data cfile preload.c]
    if {[file system $src] ne "native"} {
	set src [cache copy2 $src]
    }

    # Build the object for the helper package, 'preload' ...

    set obj [cache get preload.o]
    Compile $file $src $src $obj

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
    debug.critcl/cc {}
    # Variable used by the subst'able config setting.
    set outfile $shlib
    return [subst $em]
}

proc ::critcl::cc::CompanionObject {src} {
    debug.critcl/cc {}
    set tail    [file tail $src]
    set srcbase [file rootname $tail]
    if {[file dirname $src] ne [cache get]} {
	# The .c file does not reside in the cache directory.  Change
	# the source base so that the generated object file, which
	# will be put into the cache, does not collide with the object
	# files of other .c files with the same name.  This is done by
	# adding the last segment of the directory the file is in to
	# the base. While this can still lead to collisions the
	# probability should be low(er).
	set srcbase [file tail [file dirname $src]]_$srcbase
    }

    return [cache get ${srcbase}[config::get object]]

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

proc ::critcl::cc::GetLibraries {context} {
    debug.critcl/cc {}
    set libraries [cdefs libs? $context]

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
	set lpath [cdefs system-lib-paths $context]

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
    debug.critcl/cc {}
    foreach path $lpath {
	set f [file join $lpath $name]
	if {![file exists $f]} continue
	return $f
    }
    return -l:$name
}

proc ::critcl::cc::CompileResult {object} {
    # Variable used by the subst'able config setting.
    debug.critcl/cc {}
    set outfile $object
    return [subst [config::get output]]
}

proc ::critcl::cc::LinkResult {shlib} {
    debug.critcl/cc {}
    # Variable used by the subst'able config setting.
    set outfile $shlib

    set ldout [subst [config::get ldoutput]]
    if {$ldout eq ""} {
	set ldout [subst [config::get output]]
    }

    return $ldout
}

proc ::critcl::LibrariesOf {directories} {
    debug.critcl/cc {}
    set flag [config::get libinclude]
    set libincludes {}
    foreach dir $directories {
	lappend libincludes $flag$dir
    }
    return $libincludes
}

proc ::critcl::cc::IncludesOf {directories} {
    debug.critcl/cc {}
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
    debug.critcl/cc {}
    variable failed
    return  $failed
}

proc ::critcl::cc::StatusReset {} {
    debug.critcl/cc {}
    variable failed 0
    return
}

proc ::critcl::cc:StatusAbort? {} {
    debug.critcl/cc {}
    variable failed
    if {$failed} { return -code return }
    return
}

proc ::critcl::cc::CheckForWarnings {text} {
    debug.critcl/cc {}
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
    debug.critcl/cc {}
    variable failed

    set w [join [lassign $cmdline cmd] \n\t]
    log text \n$cmd\n\t$w\n

    set ok [config::do-log failed err [log fd] $cmdline]

    if {$ok} {
	log line [uplevel 1 [list subst $okmsg]]
    } else {
	log line [uplevel 1 [list subst $errmsg]]
	log line $err
    }

    return $ok
}

# # ## ### ##### ######## ############# #####################
## Ready
return
