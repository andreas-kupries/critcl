#!/bin/sh
#
#   Build a custom shared library using Critcl
#
#   Based originally on critbind by Jean-Claude Wippler
#   Transmogrified into critcl   by Steve Landers
#
#   $Id$
#
# \
    exec tclkit $0 ${1+"$@"}

package provide app-critcl [package require critcl]

package require cmdline
package require platform

# make sure the Tcl interpreter supports lassign
if {[info command ::lassign] eq ""} {
    proc lassign {valueList args} {
        if {[llength $args] == 0} {
            error "wrong # args: lassign list varname ?varname..?"
        }
        uplevel [list foreach $args $valueList {break}]
        return [lrange $valueList [llength $args] end]
    }
}

proc usage {args} {
    puts {To compile and run a tcl script
    critcl [-force] [-keep] file[.tcl]

To compile and build a shared library or package
    critcl options [-lib|-pkg] name [files...]

Options include:
    -debug [config|symbols|memory|all] enable debugging
    -force          force compilation of C files
    -show           show the configuration options being used
    -target target  generate binary for specified target platform/architecture
    -test           perform a simple test to check if Critcl works

Other options that may be useful:
    -I dir          adds dir to the include path when compiling
    -cache dir      sets the Critcl cache directory to dir 
    -keep           keep intermediate C files in the Critcl cache
    -config file    read the Critcl configuration options from file
    -libdir dir     location of generated library/package
    -showall        show configuration for all supported platforms
    -targets        show all available target platforms

You can display the built-in help wiki on most platforms using:
    critcl -help }
}

proc help {} {
    if {[catch {package require Mk4tcl} msg] \
            || [catch {package require Wikit} msg]} {
    puts $msg
        set txt "Couldn't load the Critcl help Wiki\n"
        append txt "To display the Critcl help wiki run \"critcl\" "
        append txt "without any options.\n"
        puts $txt
        exit
    } else {
        Wikit::init [file join $::starkit::topdir doc critcl.tkd]
   }
}

proc selftest {} {
    foreach t [glob -directory [file join $starkit::topdir test] *.tst] {
        source $t
    }
    exit
}

rename package _package
proc package {option args} {
    if {$option eq "provide"} {
        if {![catch {set v [_package present [lindex $args 0]]}] \
                && [llength $args] > 1 && $v ne [lindex $args 1]} {
            _package forget [lindex $args 0]
        }
    }
    eval _package $option $args
}

proc error {msg} {
    global argv0
    puts stderr "$argv0 error: $msg"
    exit 1
}

if {$tcl_version < 8.4} {
    # need "file normalize"
    set interp "[file tail [info nameofexecutable]] $tcl_version"
    puts stderr "Critcl requires Tcl 8.4 or later - got $interp"
    exit 1
}

set argv0 [cmdline::getArgv0]
set keep 0
set lib 0
set libdir lib
set pkg 0
set verbose 0
set keep 0
set help 0
set tcl_prefix ""
set tk_prefix ""
set args [list I.arg cache clean config.arg debug.arg force help keep lib \
               libdir.arg pkg show showall target.arg targets test]

set cleaning 0
set show 0
set showall 0
set config ""
set target ""
set targets 0
set selftest 0

while {[set result [cmdline::getopt argv $args opt arg]] != 0} {
    if {$result == -1} {
        usage $arg
        exit 1
    }
    switch $opt {
        I          { critcl::config I $arg }
        cache      { critcl::cache $arg }
        clean      { incr cleaning }
        config     { set config $arg }
        debug      { critcl::debug $arg }
        force      { critcl::config force 1 ; puts stderr "Compilation forced" }
        keep       { critcl::config keepsrc 1  ; set keep 1 }
        help       { incr help }
        lib        { incr lib  ; incr verbose }
        libdir     { set libdir $arg }
        pkg        { incr pkg ; incr verbose }
        show       { incr show }
        showall    { incr showall }
        target     { set target $arg }
        targets    { incr targets }
        test       { set selftest 1 }
    }
}

if {$help} {
    help
    exit
}

if {$config ne ""} {
    if {$argv eq "" && [file extension $config] eq ".tcl"} {
        # probably means the user has omitted the config file and we've
        # picked up the source file name
        error "-config is missing file argument"
    }
    if {![file readable $config]} {
        error "can't read Config file $config"
    }
    critcl::readconfig $config
}

if {$target ne ""} {
    if {$argv eq "" && [file extension $target] eq ".tcl"} {
        # probably means the user has omitted the config file and we've
        # picked up the source file name
        error "-target is missing file argument"
    }
    # try an exact match first
    set match [lsearch -exact -all -inline $::critcl::v::platforms $target]
    if {[llength $match] == 0} {
        # try glob if exact didn't find anything
        set match [lsearch -glob -all -inline $critcl::v::platforms $target]
    }
    switch [llength $match] {
        0 { error "unknown target $target - use one of $critcl::v::platforms" }
        1 { critcl::setconfig $match }
        default {
            error "multiple targets matched - $match"
        }
    }
}

if {$lib || $pkg} {
    critcl::crosscheck
}

if {$cleaning} {
     critcl::clean_cache
}

if {$show} {
    critcl::crosscheck
    critcl::showconfig stdout
}

if {$showall} {
    critcl::showallconfig stdout
}

if {$targets} {
    puts $critcl::v::platforms
}

if {$show || $showall || $targets} {
    exit
}

if {$selftest} {
    selftest
}

set argc [llength $argv]
if {$argc < 1} {
    if {!$cleaning} usage
    exit
}

if {$lib || $pkg} {
    set name [lindex $argv 0]
    switch [file extension $name] {
        .dll -
        .so     { set outname [file rootname $name] }
        .tcl    { set outname [file rootname $name]
                  set src $name
                }
        ""      { set outname $name }
        default { error "Not sure how to handle $name" ; usage }
    }

    if {$argc == 1} {
        set src $outname.tcl
    } else {
        eval lappend src [lrange $argv 1 end]
    }
    if {[file extension $outname] == ""} {
        append outname [critcl::sharedlibext]
    }
    critcl::config combine dynamic
} else {
    set src $argv
}

set libs {}
set objs {}
set exts {}
set inis {}
set defs {}
set ignored {}
set buildplat [::critcl::platform]
if {[info exists ::critcl::targets($buildplat)]} {
    set platform $::critcl::targets($buildplat)
} else {
    set platform $buildplat
}

set first 1
foreach f $src {
    set name [file root [file tail $f]]
    if {$name == "critcl"} continue ;# avoid reloading

    set f [critcl::file_normalize $f]

    if {$lib || $pkg} {
        set critcl::v::libsrc $f
    } else {
        set critcl::v::libsrc ""
    }

    if {![set found [file exists $f]]} {
        if {[file extension $f] != ".tcl"} {
            append f .tcl
            set found [file exists $f]
        }
        if {!$found} {
            if {!$first} { puts stderr "" }
            error "$f doesn't exist"
        }
    }
    if {$verbose} {
        if {$first} {
            puts "Target:   $platform"
            puts -nonewline "Source:   "
            set first 0
        }
        puts -nonewline "[file tail $f] " ; flush stdout
    }
    set dir [file dirname $f]
    set critcl::v::filename $f
    source $f

    # collect the names of all libraries (these have not yet been linked in)
    if {[info exists critcl::v::code($f,libs)]} {
        foreach x $critcl::v::code($f,libs) {
            if {[lsearch -exact $libs $x] < 0} {
                lappend libs $x
            }
        }
    }

    if {$critcl::v::libfile == "" && ![info exists critcl::v::code($f,list)]} {
        # puts stderr "\ncritcl::v::code($f,list) doesn't exist"
        puts stderr "nothing to build for $f"
        continue
    }

    if {$lib || $pkg} {
        if {$critcl::v::libfile == ""} {
            lassign [critcl::cbuild $f 0 ns] libfile ininame
        } else {
            set libfile $critcl::v::libfile
            set ininame $critcl::v::ininame
        }
        lappend objs $libfile
        append exts "extern Tcl_AppInitProc ${ininame}_Init;\n"
        append inis "if (${ininame}_Init(ip) != TCL_OK) return TCL_ERROR;\n"
        append defs "Tcl_StaticPackage(NULL, \"$name\", ${ininame}_Init, NULL);\n"
    }
}

if {!$critcl::v::failed && ($lib || $pkg)} {
    # create a single merged shared library

    puts "\nLibrary:  $outname"
    critcl::config combine ""
    eval critcl::clibraries $objs $libs

    # make sure the init routine ends up with the right name
    regexp {^\w+} [file tail [info script]] origname
    set origname [string totitle $origname]_Init
    regexp {^\w+} [file tail $outname] targetname
    set targetname [string totitle $targetname]_Init
    critcl::ccode "#define $origname $targetname"
    critcl::cinit $inis $exts
    set maininfo [critcl::cbuild "" 0]
    if {!$critcl::v::failed} {
        if {$pkg} {
            set libname [file rootname [file tail $outname]]
            set libdir [file normalize $libdir]
            if {[file isfile $libdir]} {
                error "can't package $outname - $libdir is not a directory"
            } elseif {![file isdirectory $libdir]} {
                file mkdir $libdir
            }
            set version [package present $libname]
            set pkgdir [file join $libdir $libname]
            set libdir [file join $pkgdir $platform]
            if {![file isdirectory $libdir]} {
                file mkdir $libdir
            }
            # handle preloaded libraries
            set preload ""
            if {[llength $critcl::v::preload]} {
                set ext $critcl::c::sharedlibext
                foreach p $critcl::v::preload {
                    set tail [file tail $p]
                    # look for lib as follows:
                    #   - lib.so
                    #   - dir/lib.so
                    #   - dir/plat/lib.so
                    set pre $tail$ext
                    if {![file readable $pre]} {
                        if {[file isdirectory $p]} {
                            set dir $p
                        } else {
                            set dir [file dirname $p]
                            if {$dir eq "."} {
                                set dir $tail
                            }
                        }
                        set pre [file join $dir $tail$ext]
                        if {![file readable $pre]} {
                            set pre [file join $dir $platform $tail$ext]
                            if {![file readable $pre]} {
                                set msg "can't find preload library $tail$ext"
                                append msg " for $platform"
                                error $msg
                            }
                        }
                    }
                    file copy -force $pre $libdir
                    lappend preload $tail
                }
                set prelib [file join $critcl::v::cache preload$ext]
                file copy -force $prelib $libdir
                puts "Preload:  [join $preload {, }]"
            }
            if {[string first [pwd] $pkgdir] != -1} {
                set first [string length [pwd]]
                set dir [string range $pkgdir [incr first] end]
            } else {
                set dir $pkgdir
            }
            puts "Package:  $dir"

            # build pkgIndex.tcl
            set index [open [file join $pkgdir pkgIndex.tcl] w]
            puts $index {source [file join $dir critcl.tcl]}
            puts $index "critcl::loadlib \$dir $libname $version $preload"
            close $index

            # arrange for each Tcl source file (specified by critcl::tsources)
            # to be copied into the Tcl subdirectory (in accordance with TIP 55)
            if {[info exists critcl::v::tsources]} {
                file mkdir [set tcldir [file join $pkgdir Tcl]]
                foreach t $critcl::v::tsources {
                    file copy -force $t $tcldir
                }
            }

            # update the platform map

            # first read in old one from critcl.tcl - this code is dependent
            # on the structure of proc platform so be careful when changing
            set file [file join $pkgdir critcl.tcl]
            if {[file readable $file]} {
                set fd [open $file]
                set pattern "set ::critcl::mapping \{"
                set len [string length $pattern]
                while {[gets $fd line] != -1} {
                    set line [string trim $line]
                    if {[string first $pattern $line] != -1} {
                        if {![info complete $line]} {
                            error \
                    "invalid ::critcl::mapping command in runtime critcl.tcl"
                        }
                        eval $line
                        foreach {config plat} $::critcl::mapping {
                            set mapping($config) $plat
                        }
                        break
                    }
                }
                close $fd
            }
            if {[string match $::critcl::v::config $::critcl::v::platform]} {
                # if the current platform matches a config then we don't
                # need to map so we unset it in case a mapping was inherited
                # from a previous critcl invocation 
                array unset mapping $::critcl::v::config
            } 
            # create a mapping for each of the platforms listed on
            # the Config platform line
            set map [interp eval $::critcl::run subst \"$::critcl::c::platform\"]
            set minver [lindex $map 1]
            set plats [list]
            foreach plat [lrange $map 2 end] {
                set mapping($plat) [list $::critcl::v::platform $minver]
                lappend plats $plat
            }
            if {[llength $plats]} {
                puts "Platform: [join $plats {, }] $minver and later"
            }

            # create the critcl.tcl file in the generated package. This
            # provides the library loading code, plus no-ops for any
            # Critcl procs so that both C and Tcl source can be in
            # the one file

            set runtime [file join [file dirname [info script]] runtime.tcl]
            if {[file readable runtime]} {
                error "can't find Critcl runtime.tcl"
            }
            set fd [open $runtime]
            set txt [read $fd]
            close $fd

            # append dummy Critcl procs
            append txt "\n# dummy Critcl procs\n"
            append txt "namespace eval ::critcl {\n"
            foreach proc [namespace eval ::critcl {namespace export}] {
                switch $proc {
                    platform  { # we provide one in the runtime script }
                    compiled  { append txt "  proc compiled args {return 1}\n" }
                    compiling { append txt "  proc compiling args {return 0}\n" }
                    done      { append txt "  proc done args {return 1}\n" }
                    check     { append txt "  proc check args {return 0}\n" }
                    default {
                        append txt "  proc $proc args {}\n"
                    }
                }
            }
            append txt "}\n"

            # create a version of critcl::platform that applies the platform map 

            # clone the ::platform::generic command
            append txt "\n# a clone of platform::generic\n"
            append txt "namespace eval ::platform {\n"
            append txt "    proc generic {} {"
            append txt [info body ::platform::generic]
            append txt "\n    }\n}\n\n"

            # write the new mapping to the end of the runtime file
            append txt "# runtime platform mapping - please do not edit\n"
            append txt "set ::critcl::mapping {"
            foreach plat [lsort [array names mapping]] {
                append txt "$plat [list $mapping($plat)] "
            }
            append txt "}\n"

            set fd [open [file join $pkgdir critcl.tcl] w]
            puts $fd $txt
            close $fd

            set outname [file join $libdir [file tail $outname]]
            array unset mapping
        }
        if {[file exists $outname]} {file delete -force $outname}
        file copy [lindex $maininfo 0] $outname

        # For MSVC debug builds we get a separate debug info file.
        set pdb [file root [lindex $maininfo 0]].pdb
        if {[file exists $pdb]} {
            file copy -force $pdb [file root $outname].pdb
        }
    }
}
if {$keep || $critcl::v::failed} {
     puts stderr "Files left in $critcl::v::cache"
}

