#!/bin/sh
#
#   Build a custom shared library using CriTcl
#
#   Based originally on critbind by Jean-Claude Wippler
#   Transmogrified into critcl   by Steve Landers
#
#   $Id$
#
# \
    exec tclkit $0 ${1+"$@"}

package provide app-critcl 1.0

set critver [package require critcl]

package require cmdline

proc usage {args} {
    global argv0
    #package require Wikit
    if {[catch {package require Mk4tcl} msg] ||
        [catch {package require Wikit} msg]} {
        if {$args != ""} {
            if {[llength $args] == 1} {
                set args [lindex $args 0]
            }
            puts stderr "\n$argv0: $args"
        }
        puts stderr {
        To compile and run a tcl script
            critcl [-force] [-keep] file[.tcl]

        To compile and build a shared library or package
            critcl [-force] [-keep] [-lib|-pkg] name [infiles...]

        -force      force compilation (ignores cache)
        -keep       keep intermediate C files
        -lib        generate $name.so
        -pkg        generate a package structure under lib/$name

        all options may be abbreviated to the first character
      }
   } else {
        Wikit::init [file join $::starkit::topdir doc critcl.tkd]
   }
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
set tcl_prefix ""
set tk_prefix ""
set args [list f force k keep l lib L.arg libdir.arg p pkg I.arg tk]
while {[set result [cmdline::getopt argv $args opt arg]] != 0} {
    if {$result == -1} {
        usage $arg
        exit 1
    }
    switch $opt {
        f -
        force  { critcl::config force 1
                 puts stderr "Compilation forced"
               }
        k -
        keep   { critcl::config keepsrc 1
                 set keep 1
               }
        l -
        lib    { incr lib  ; incr verbose }
        libdir { set libdir $arg }
        p -
        pkg    { incr pkg ; incr verbose }
        I      { critcl::config I $arg }
        tk     { critcl::tk }
        tcl_prefix { set tcl_prefix $arg }
        tk_prefix { set tk_prefix $arg }
    }
}

if {$keep} {
     puts stderr "Files kept in [critcl::cache]"
}

set argc [llength $argv]
if {$argc < 1} {
    usage
    exit
}

if {$lib || $pkg} {
    critcl::crosscheck
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
            puts -nonewline stderr "Source: "
            set first 0
        }
        puts -nonewline stderr "[file tail $f] " ; flush stdout
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
        puts stderr "\ncritcl::v::code($f,list) doesn't exist"
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

    puts stderr "\nLibrary: $outname"
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
        set libdir [file join $pkgdir [critcl::platform]]
        if {![file isdirectory $libdir]} {
            file mkdir $libdir
        }
        puts stderr "Package: $pkgdir"

        # build pkgIndex.tcl
        set index [open [file join $pkgdir pkgIndex.tcl] w]
        puts $index {source [file join $dir critcl.tcl]}
        puts $index "critcl::loadlib \$dir $libname $version"
        close $index

        # arrange for each Tcl source file (specified by critcl::tsources)
        # to be copied into the Tcl subdirectory (in accordance with TIP 55)
        if {[info exists critcl::v::tsources]} {
            file mkdir [set tcldir [file join $pkgdir Tcl]]
            foreach t $critcl::v::tsources {
                file copy -force $t $tcldir
            }
        }
        
        # create the critcl.tcl file in the generated package. This
        # provides the library loading code, plus no-ops for any
        # critcl functions so that both C and Tcl source can be in
        # the one file

        set    txt "\n"
        append txt "#\n"
        append txt "#   CriTcl - build C extensions on-the-fly\n"
        append txt "#\n"
        append txt "#   Copyright (c) 2001-2004 Jean-Claude Wippler\n"
        append txt "#   Copyright (c) 2002-2004 Steve Landers\n"
        append txt "#\n"
        append txt "#   See http://www.purl.org/tcl/wiki/critcl\n"
        append txt "\n"
        append txt "\nnamespace eval ::critcl {\n"
        foreach p [list check config cheaders csources clibraries cinit \
                        ccode ccommand cproc cdata tk tsources cheaders \
			cdefines] {
            append txt "  proc $p args {}\n"
        }
        append txt "  proc done args {return 1}\n"
        append txt "  proc check args {return 0}\n"
        append txt "\n"
        append txt "  proc loadlib {dir package version} \{\n"
        append txt "    global tcl_platform\n"
        append txt {    set path [file join $dir [critcl::platform]]}
        append txt "\n"
        append txt {    set lib [file join $path $package}
        append txt {[info sharedlibextension]]}
        append txt "\n"
        append txt {    set plat [file join $path critcl.tcl]}
        append txt "\n"
        append txt {    set provide "package provide $package $version"}
        append txt "\n"
        append txt {    append provide "; [list load $lib $package]; [list source $plat]"}
        append txt "\n"
        append txt {    foreach t [glob -nocomplain}
        append txt { [file join $dir Tcl *.tcl]]}
        append txt " {\n"
        append txt {      append provide "; [list source $t]"}
        append txt "\n"
        append txt "    }\n"
        append txt {    package ifneeded $package $version $provide}
        append txt "\n"
        append txt "    package ifneeded critcl 0.0"
        append txt { "package provide critcl 0.0; [list source [file join $dir critcl.tcl]]"}
        append txt "\n"
        append txt "  \}\n"

        # place a copy of the critcl::platform proc at the end of critcl.tcl
        append txt "\n  proc platform {[info args ::critcl::platform]}"
        append txt " {[info body ::critcl::platform]}"
        append txt "\n}\n"
        set fd [open [file join $pkgdir critcl.tcl] w]
        puts $fd $txt
        close $fd

        # save platform specific parameters for runtime
        set txt "# CriTcl parameters for [critcl::platform]\n"
        append txt "namespace eval critcl {\n"
        append txt "  proc compiling args {return [critcl::compiling]}\n"
        append txt "  proc scripting args {return [critcl::scripting]}\n"
        append txt "  proc failed args {return $critcl::v::failed}\n"
        append txt "}\n"
        set fd [open [file join $libdir critcl.tcl] w]
        puts $fd $txt
        close $fd

        set outname [file join $libdir [file tail $outname]]
    }
    file delete $outname
    file copy [lindex $maininfo 0] $outname
}
