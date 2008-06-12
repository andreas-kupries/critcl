package provide critcl 2.0

package require platform 1.0.2

set sourcedir [info script]

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

# md5 could be a cmd or a pkg, or be in a separate namespace
if {[catch { md5 "" }]} {
  # do *not* use "package require md5c" since crtcl is not loaded yet,
  # but do look for a compiled one, in case object code already exists
  if {![catch { md5c "" }]} {
    interp alias {} md5 {} md5c
  } elseif {[catch {package require Trf 2.0}] || [catch {::md5 -- test}]} {
    # else try to load the Tcl version in tcllib
    catch { package require md5 }
    if {![catch { md5::md5 "" }]} {
      interp alias {} md5 {} md5::md5
    } else {
      # last resort: package require or source Don Libes' md5pure script
      if {[catch { package require md5pure }]} {
	if {[file exists md5pure.tcl]} {
	  source md5pure.tcl
	  interp alias {} md5 {} md5pure::md5
	} else {
	  source [file join [file dirname [info script]] ../md5/md5.tcl]
	  interp alias {} md5 {} md5::md5
	}
      } else {
	interp alias {} md5 {} md5pure::md5
      }
    }
  }
}

namespace eval ::critcl {
  namespace export cache ccode ccommand cdata cdefines cflags cheaders \
                   check cinit clibraries compiled compiling config cproc \
                   csources debug done failed framework ldflags platform \
                   tk tsources preload license
  variable run [interp create] ;# interpreter to run commands, eval when, etc

  # ouch, some md5 implementations return hex, others binary
  if {[string length [md5 ""]] == 32} {
    proc md5_hex {s} { return [md5 $s] }
  } else {
    proc md5_hex {s} { binary scan [md5 $s] H* md; return $md }
  }

  # file normalize is a Tcl 8.4 extension, emulate it if not available
  if {[catch { file normalize . }]} {
    proc file_normalize {file} {
      set sp [file split $file]
      if {[file pathtype [lindex $sp 0]] == "relative"} {
	set sp [file split [eval [list file join [pwd]] $sp]]
      }
      set np {}
      foreach ele $sp {
	if {$ele != ".."} {
	  if {$ele != "."} { lappend np $ele }
	} elseif {[llength $np]> 1} {
	  set np [lrange $np 0 [expr {[llength $np] - 2}]]
	}
      }
      if {[llength $np] > 0} { return [eval file join $np] }
    }
  } else {
    proc file_normalize {file} { return [file normalize $file] }
  }

    proc platform {} {
        return $v::platform
    }

    proc platformcc {} {
        set platform [::platform::generic]
        if {[string match "win32-*" $platform]} {
            set cc gcc
            if {[info exists ::env(CC)]} {
                set cc $::env(CC)
            } elseif {[string length [auto_execok cl]] > 0} {
                set cc cl
            }
            append platform -$cc
        }
        return $platform
    }

    proc cache {{dir ""}} {
        if {$dir ne ""} {
            set v::cache [file normalize $dir]
        }
        return $v::cache
    }

    # namespace to flag when options set
    namespace eval option {
        variable debug_symbols  0
    }

  # keep all variables in a sub-namespace for easy access
  namespace eval v {
    variable cache                          ;# cache directory
    variable platform                       ;# target platform
    variable version ""                     ;# min version # on platform
    variable generic [critcl::platformcc]   ;# actual platform
    variable config                         ;# the matching config
    variable hdrdir	[file join [file dirname [info script]] critcl_c]

    variable prefix	"v[package require critcl]"
    regsub {\.} $prefix {} prefix

    variable compile
    variable link
    variable options

    array set options {outdir "" keepsrc 0 combine "" appinit "" force 0}
    array set options {I "" L "" tk 0 language "" lines 1}

    variable code	     ;# this array collects all code snippets
    variable curr	     ;# current digest
    variable compiling 0 ;# indicates that the gcc/cc is available
    variable failed 0    ;# set if compile fails
    variable ininame ""
    variable libfile ""
    variable cflags ""
    variable ldflags ""
    variable targets ""    ;# cross-compile targets
    variable objs [list] ;# compiled object for each csources
    variable preload ""  ;# list of shared libraries to pre-load
    variable libsrc ""

    # config variables
    variable configvars { platform compile include link strip tclstubs tkstubs
                          debug_memory debug_symbols output preproc_define
                          preproc_enum object optimize noassert threadflags
                          sharedlibext link_debug link_release version
                          link_preload ldoutput
                        }

  }

  # keep config options in a namespace
  namespace eval c [list
      foreach var $v::configvars {
        variable $var
     }
  ]

  proc emit {s} {
    append v::code($v::curr) $s
  }

  proc emitln {{s ""}} {
    emit "$s\n"
  }

  proc config {option args} {
    if {![info exists v::options($option)] || [llength $args] > 1} {
      error "option must be one of: [lsort [array names v::options]]"
    }
    if {[llength $args] == 0} { return $v::options($option) }
    set v::options($option) [lindex $args 0]
  }

  proc setparam {type list} {
    set digest [md5_hex "$type $list"]
    set file [file_normalize [info script]]

    lappend v::code($file,list) "" $digest ;# add so we can detect changes

    upvar 0 v::code($file,$type) param
    if {[llength $list] > 0} {
        set base [file dirname $file]
        foreach x $list {
            if {[string index $x 0] == "-"} {
                lappend param $x
            } else {
                foreach y [glob [file join $base $x]] {
                    set z [file_normalize $y]
                    if {![file exists $z]} { error "$z: not found" }
                    lappend param $z
                }
            }
        }
    } elseif {[info exists param]} {
        return $param
    }
  }

  proc cheaders {args} {
    return [setparam hdrs $args]
  }

  proc csources {args} {
    return [setparam srcs $args]
  }

  proc clibraries {args} {
    return [setparam libs $args]
  }

  proc framework {args} {
      foreach arg $args {
          # if an arg contains a slash it must be a framework path
          if {[string first / $arg] == -1} {
              append c::link " -framework $arg"
          } else {
              append c::compile " -F$arg"
              append c::link " -F$arg"
          }
      }
  }

  proc cflags {args} {
      foreach arg $args {
        append v::cflags " $arg"
      }
  }

  proc include {args} {
      foreach arg $args {
          append v::cflags "$c::include$arg"
      }
  }

  proc ldflags {args} {
      foreach arg $args {
	# protect any whitespace in the arg - mainly for directory names
	regsub -all {(\s)} $arg {\\&} arg 
        append v::ldflags " -Wl,$arg"
      }
  }

  proc cinit {text exts} {
    set digest [md5_hex $text]
    set file [file_normalize [info script]]

    lappend v::code($file,list) "" $digest ;# add so we can detect changes

    append v::code($file,init) $text \n
    append v::code($file,ext) $exts \n
  }

  proc ccode {text} {
    set digest [md5_hex $text]
    set file [file_normalize [info script]]

    lappend v::code($file,list) "" $digest ;# add so we can detect changes

    set v::code($digest) ""
    if {$v::options(lines)} {
      append v::code($digest) "#line 1 \"[file tail $file]\"\n"
    }
    append v::code($digest) $text \n
    append v::code(ccode) $text \n
  }

  proc define {ns name args} {
    set v::curr [md5_hex "$ns $name $args"]
    set file [file_normalize [info script]]
    set cns [string map {:: _} $ns]

    set ::auto_index($ns$name) [list [namespace current]::cbuild $file]
    #if {[info commands $name] != ""} { rename $name "" }

    lappend v::code($file,list) $cns$name $v::curr
    set v::code($v::curr) "#define ns_$cns$name \"$ns$name\"\n"
    if {$v::options(lines)} {
      append v::code($v::curr) "#line 1 \"[file tail $file]/$name\"\n"
    }
  }

  proc ccommand {name anames args} {
    set ns [uplevel 1 namespace current]
    if {$ns == "::"} { set ns "" } else { append ns :: }
    set cns [string map {:: _} $ns]

    define $ns $name $anames $args

    set clientdata NULL
    set delproc 0
    while {[string match "-*" $args]} {
        switch -- [lindex $args 0] {
          -clientdata { set clientdata [lindex $args 1] }
          -delproc { set delproc [lindex $args 1] }
        }
        set args [lrange $args 2 end]
    }
    set v::clientdata($name) $clientdata
    set v::delproc($name) $delproc
    set body $args
    if {$body != ""} {
      lappend anames ""
      foreach {cd ip oc ov} $anames break
      if {$cd == ""} { set cd clientdata }
      if {$ip == ""} { set ip interp }
      if {$oc == ""} { set oc objc }
      if {$ov == ""} { set ov objv }

      set ca "(ClientData $cd, Tcl_Interp *$ip, int $oc, Tcl_Obj *CONST $ov\[])"

      emitln "static int"
      emitln "tcl_$cns$name$ca"
      emitln \{
      emit $body
      emitln \}
    } else {
      # if no body is specified, then $anames is alias for the real cmd proc
      emitln "#define tcl_$cns$name $anames"
      emitln "int $anames\(\);"
    }
    unset v::curr
  }

  proc cproc {name adefs rtype {body "#"}} {
    set ns [uplevel 1 namespace current]
    if {$ns == "::"} { set ns "" } else { append ns :: }
    set cns [string map {:: _} $ns]

    define $ns $name $adefs $rtype $body

    set cname c_$cns$name
    set wname tcl_$cns$name

    array set types {}
    set names {}
    set cargs {}
    set cnames {}

    # is first arg is "Tcl_Interp*", pass it without counting it as a cmd arg
    if {[lindex $adefs 0] == "Tcl_Interp*"} {
      lappend cnames ip
      lappend cargs [lrange $adefs 0 1]
      set adefs [lrange $adefs 2 end]
    }

    foreach {t n} $adefs {
      set types($n) $t
      lappend names $n
      lappend cnames _$n
      if {$t eq "bytearray" || $t eq "rawchar*"} {
          lappend cargs "char * $n"
      } else {
          lappend cargs "$t $n"
      }
    }

    switch -- $rtype {
      ok      { set rtype2 "int" }
      string -
      dstring -
      vstring { set rtype2 "char*" }
      default { set rtype2 $rtype }
    }

    if {$body != "#"} {
      emitln "static $rtype2"
      emitln "${cname}([join $cargs {, }])"
      emit \{
      emit $body
      emitln \}
    } else {
      emitln "#define $cname $name"
    }

    set ca "(ClientData cd, Tcl_Interp *ip, int oc, Tcl_Obj *CONST ov\[])"

    emitln
    emitln "static int"
    emitln "$wname$ca"
    emitln \{

    foreach x $names {
      set t $types($x)
      switch -- $t {
	int - long - float - double -
        char* - int* - float* - double* -
        Tcl_Obj* {
	  emitln "  $types($x) _$x;"
	}
        bytearray -
        rawchar* {
            emitln "  char* _$x;"
        }
	default {
	  emitln "  void *_$x;"
	}
      }
    }

    if {$rtype != "void"} { emit "  $rtype2 rv;" }

    emitln "
  if (oc != [expr {[llength $names] + 1}]) {
    Tcl_WrongNumArgs(ip, 1, ov, \"[join $names { }]\");
    return TCL_ERROR;
  }
"
    set n 0
    foreach x $names {
      incr n
      switch -- $types($x) {
	int {
	  emitln "  if (Tcl_GetIntFromObj(ip, ov\[$n], &_$x) != TCL_OK)"
	  emitln "    return TCL_ERROR;"
	}
	long {
	  emitln "  if (Tcl_GetLongFromObj(ip, ov\[$n], &_$x) != TCL_OK)"
	  emitln "    return TCL_ERROR;"
	}
	float {
	  emitln "  { double t;"
	  emitln "    if (Tcl_GetDoubleFromObj(ip, ov\[$n], &t) != TCL_OK)"
	  emitln "      return TCL_ERROR;"
	  emitln "    _$x = (float) t;"
	  emitln "  }"
	}
	double {
	  emitln "  if (Tcl_GetDoubleFromObj(ip, ov\[$n], &_$x) != TCL_OK)"
	  emitln "    return TCL_ERROR;"
	}
	char* {
	  emitln "  _$x = Tcl_GetString(ov\[$n]);"
	}
        int* -
        float* -
        double* {
          emitln "  _$x = ($types($x)) Tcl_GetByteArrayFromObj(ov\[$n], NULL);"
          emitln "  Tcl_InvalidateStringRep(ov\[$n]) ;"
        }
        bytearray -
        rawchar* {
            emitln "  _$x = (char*) Tcl_GetByteArrayFromObj(ov\[$n], NULL);"
            emitln "  Tcl_InvalidateStringRep(ov\[$n]) ;"
        }
	default {
	  emitln "  _$x = ov\[$n];"
	}
      }
    }
    emitln

    emit "  ";
    if {$rtype != "void"} { emit "rv = " }
    emitln "${cname}([join $cnames {, }]);"
    emitln

    switch -- $rtype {
      void    	{ }
      ok	{ emitln "  return rv;" }
      int	{ emitln "  Tcl_SetObjResult(ip, Tcl_NewIntObj(rv));" }
      long	{ emitln "  Tcl_SetObjResult(ip, Tcl_NewLongObj(rv));" }
      float -
      double	{ emitln "  Tcl_SetObjResult(ip, Tcl_NewDoubleObj(rv));" }
      char*	{ emitln "  Tcl_SetResult(ip, rv, TCL_STATIC);" }
      string -
      dstring	{ emitln "  Tcl_SetResult(ip, rv, TCL_DYNAMIC);" }
      vstring	{ emitln "  Tcl_SetResult(ip, rv, TCL_VOLATILE);" }
      default 	{ emitln "  Tcl_SetObjResult(ip, rv); Tcl_DecrRefCount(rv);" }
    }
    if {$rtype != "ok"} { emitln "  return TCL_OK;" }
    emitln \}

    unset v::curr
  }

  proc cdata {name data} {
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

    uplevel [list critcl::ccommand $name {dummy ip objc objv} "
  static char script\[$count] = {
    $inittext
  };
  Tcl_SetByteArrayObj(Tcl_GetObjResult(ip), (unsigned char*) script, $count);
  return TCL_OK;
"]

    return $name
  }

    proc compile {file src copts lfd obj} {
        variable run
        set cmdline "$c::compile $v::cflags $c::threadflags $c::tclstubs $copts"
        set outfile $obj
        append cmdline " [subst $c::output] [list $src]"
        if {$v::options(language) != ""} {
         # Allow the compiler to determine the type of file
         # otherwise it will try to compile the libs
         append cmdline " -x none"
        }
        # add the Tk stuff
        if {$v::options(tk)} {
            append cmdline " $c::tkstubs"
        }
        if {!$option::debug_symbols} {
            append cmdline " $c::optimize $c::noassert"
        }
        if {$v::options(combine) == "standalone"} {
            regsub $c::tclstubs $cmdline { } cmdline
            regsub $c::tkstubs $cmdline { } cmdline
        }
        puts $lfd $cmdline
        set v::failed 0
        interp transfer {} $lfd $run
        if {[catch {
            interp eval $run "exec $cmdline 2>@ $lfd"
            interp transfer $run $lfd {}
            if {!$v::options(keepsrc) && $src ne $file} { file delete $src }
            puts $lfd "$obj: [file size $obj] bytes"
        } err]} {
            interp transfer $run $lfd {}
            puts $lfd "ERROR while compiling code in $file:"
            puts $lfd $err
            incr v::failed
        }
    }

  proc cbuild {{file ""} {load 1} {prefix {}} {silent ""}} {
    if {$file eq ""} {
        set link 1
        set file [file_normalize [info script]]
    } else {
        set link 0
    }

    # each unique set of cmds is compiled into a separate extension
    set digest [md5_hex "$file $v::code($file,list)"]

    set cache $v::cache
    set cache [file_normalize $cache]

    set base [file join $cache ${v::prefix}_$digest]
    set libfile $base

    # the compiled library will be saved for permanent use if the outdir
    # option is set (in which case rebuilds will no longer be automatic)
    if {$v::options(outdir) != ""} {
      set odir [file join [file dirname $file] $v::options(outdir)]
      set oroot [file root [file tail $file]]
      set libfile [file_normalize [file join $odir $oroot]]
      file mkdir $odir
    }
    # get the settings for this file into local variables
    foreach x {hdrs srcs libs init ext} {
      set $x [append v::code($file,$x) ""] ;# make sure it exists
    }

    # modify the output file name if debugging symbols are requested
    if {$option::debug_symbols} {
        append libfile _g
    }

    # choose distinct suffix so switching between them causes a rebuild
    switch -- $v::options(combine) {
      ""         -
      dynamic    { append libfile _pic$c::object }
      static     { append libfile _stub$c::object }
      standalone { append libfile $c::object }
    }

    # the init proc name takes a capitalized prefix from the package name
    set ininame stdin ;# in case it's called interactively
    regexp {^\w+} [file tail $file] ininame
    set ininame [string totitle $ininame]
    if {$prefix != {}} {
        set ininame "${prefix}_$ininame"
    }

    # the shared library we hope to produce
    set target $base$c::sharedlibext
    if {$v::options(force) || ![file exists $target]} {
      file mkdir $cache

      set log [file join $cache [pid].log]
      set lfd [open $log w]
      puts $lfd "\n[clock format [clock seconds]] - $file"

      set fd [open $base.c w]
      set names {}

      puts $fd "/* Generated by critcl on [clock format [clock seconds]]
 * source: $file
 * binary: $libfile
 */
#include \"tcl.h\"\n"

        if {$v::options(tk)} {
          puts $fd "\n#include \"tk.h\""
        }

      foreach {name digest} $v::code($file,list) {
	if {[info exists v::code($digest)]} {
	  puts $fd "/* [string repeat - 70] */\n"
	  puts $fd $v::code($digest)
	  if {$name != ""} {
	    lappend names $name
	  }
	}
      }
      puts $fd "/* [string repeat - 70] */"

      puts -nonewline $fd {
# line 1 "MyInitTclStubs"

#if USE_TCL_STUBS
  TclStubs *tclStubsPtr;
  TclPlatStubs *tclPlatStubsPtr;
  struct TclIntStubs *tclIntStubsPtr;
  struct TclIntPlatStubs *tclIntPlatStubsPtr;

  static int
  MyInitTclStubs (Tcl_Interp *ip)
  {
    typedef struct {
      char *result;
      Tcl_FreeProc *freeProc;
      int errorLine;
      TclStubs *stubTable;
    } HeadOfInterp;

    HeadOfInterp *hoi = (HeadOfInterp*) ip;

    if (hoi->stubTable == NULL || hoi->stubTable->magic != TCL_STUB_MAGIC) {
      ip->result = "This extension requires stubs-support.";
      ip->freeProc = TCL_STATIC;
      return 0;
    }

    tclStubsPtr = hoi->stubTable;

    if (Tcl_PkgRequire(ip, "Tcl", "8.1", 0) == NULL) {
      tclStubsPtr = NULL;
      return 0;
    }

    if (tclStubsPtr->hooks != NULL) {
	tclPlatStubsPtr = tclStubsPtr->hooks->tclPlatStubs;
	tclIntStubsPtr = tclStubsPtr->hooks->tclIntStubs;
	tclIntPlatStubsPtr = tclStubsPtr->hooks->tclIntPlatStubs;
    }

    return 1;
  }
#endif
}
      if {$v::options(tk)} {
        setup_tk_stubs $fd
      }
      puts $fd "
#ifdef __cplusplus
extern \"C\" {
#endif
      $ext
DLLEXPORT int
${ininame}_Init(Tcl_Interp *ip)
{
#if USE_TCL_STUBS
  if (!MyInitTclStubs(ip)) return TCL_ERROR;
#endif"
      if {$v::options(tk)} {
        puts $fd "
# line 1 \"MyInitTkStubs\"
#if USE_TK_STUBS
  if (!MyInitTkStubs(ip)) return TCL_ERROR;
#endif"
      }
      puts $fd "$init "
      if {[info exists v::defines]} {
        build_defines $fd $file
      }
      foreach x [lsort $names] {
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
        puts $fd "  Tcl_CreateObjCommand(ip, ns_$x, tcl_$x, $cd, $dp);"
      }
      puts $fd "  return TCL_OK;
}
#ifdef __cplusplus
}
#endif"
      close $fd

      foreach x [glob -directory $v::hdrdir *.h] {
        set fn [file join $cache [file tail $x]]
	if {![file exists $fn]} {
	  file copy $x $fn
	}
      }

      # copy X11 stuff on Windows
      if {$v::options(tk) && $::tcl_platform(platform) eq "windows"} {
          set xdir [file join $cache X11]
          if {![file isdirectory $xdir]} {
              file copy [file join $v::hdrdir X11] $cache
          }
      }

      set copts [list]
      if {$v::options(language) != "" && [file tail $file] ne "critcl.tcl"} {
	lappend copts -x $v::options(language)
      }
      if {$v::options(I) != ""} {
	lappend copts $c::include$v:::options(I)
      }
      lappend copts $c::include$cache
      set copies {}
      foreach x $hdrs {
	if {[string index $x 0] == "-"} {
	  lappend copts $x
	} else {
	  set copy [file join $cache [file tail $x]]
	  file delete $copy
	  file copy $x $copy
	  lappend copies $copy
	}
      }

      compile $file $base.c $copts $lfd $libfile
      foreach src $srcs {
          set tail [file tail $src]
          set srcbase [file rootname [file tail $src]]
          if {[file dirname $base] ne [file dirname $src]} {
              set srcbase [file tail [file dirname $src]]_$srcbase
          }
          set obj [file join [file normalize $cache] ${srcbase}$c::object]
          compile $src $src $copts $lfd $obj
          lappend v::objs $obj
      }
      if {($load || $link) && !$v::failed} {
        set cmdline $c::link
        if {[llength $v::preload]} {
            append cmdline " $c::link_preload"
        }
        set outfile $target
        if {[string length [set ldout [subst $c::ldoutput]]] == 0} {
            set ldout [subst $c::output]
        }
        if {$option::debug_symbols} {
            append cmdline " $c::link_debug $ldout"
        } else {
            append cmdline " $c::strip $c::link_release $ldout"
        }
        if {[string match "win32-*-cl" [platformcc]]} {
            regsub -all -- {-l(\S+)} $libs {\1.lib} libs
        }
        append cmdline " [list $libfile] "
        if {[string match "win32-*-cl" [platformcc]]} {
            set f [open [set rsp [file join $cache link.fil]] w]
            puts $f [join $v::objs \n]
            close $f
            append cmdline @$rsp
        } else {
            append cmdline $v::objs
        }
        append cmdline " $libs $v::ldflags"
#       append cmdline " bufferoverflowU.lib";# msvc >=1400 && <1500 for amd64
        puts $lfd "\n$cmdline"
        variable run
        interp transfer {} $lfd $run
        if {[catch {
            interp eval $run "exec $cmdline 2>@ $lfd"
            interp transfer $run $lfd {}
            puts $lfd "$target: [file size $target] bytes"
        } err]} {
            interp transfer $run $lfd {}
            puts $lfd "ERROR while linking $target:"
            incr v::failed
        }
        if {!$v::failed && [llength $v::preload]} {
            # compile preload if necessary
            set outfile [file join [file dirname $base] \
                            preload$c::sharedlibext]
            if {![file exists $outfile]} {
                set src [file join $v::cache preload.c]
                set obj [file join $v::cache preload.o]
                compile $src $src $copts $lfd $obj
                set cmdline "$c::link [list $obj] $c::strip [subst $c::output]"
                puts $lfd "\n$cmdline"
                interp transfer {} $lfd $run
                if {[catch {
                    interp eval $run "exec $cmdline 2>@ $lfd"
                    interp transfer $run $lfd {}
                    puts $lfd "$outfile: [file size $target] bytes"
                } err]} {
                    interp transfer $run $lfd {}
                    puts $lfd "ERROR while linking $outfile:"
                    incr v::failed
                }
            }
        }
      }
      # read build log
      close $lfd
      set lfd [open $log]
      set msgs [read $lfd]
      close $lfd
      file delete -force $log
      # append to critcl log
      set log [file join $cache $v::prefix.log]
      set lfd [open $log a]
      puts $lfd $msgs
      close $lfd
      foreach x $copies { file delete $x }
    }

    if {$v::failed} {
      if {$silent == ""} {
	puts stderr $msgs
	puts stderr "critcl build failed ($file)"
      }
    } elseif {$load} {
        load $target $ininame
    }

    foreach {name digest} $v::code($file,list) {
      if {$name != "" && [info exists v::code($digest)]} {
	unset v::code($digest)
      }
    }
    foreach x {hdrs srcs init} {
      array unset v::code $file,$x
    }
    if {$link} {
      return [list $target $ininame]
    }
    return [list $libfile $ininame]
  }

  proc setup_tk_stubs fd {
      puts -nonewline $fd {
#if USE_TK_STUBS

    TkStubs *tkStubsPtr;
    struct TkPlatStubs *tkPlatStubsPtr;
    struct TkIntStubs *tkIntStubsPtr;
    struct TkIntPlatStubs *tkIntPlatStubsPtr;
    struct TkIntXlibStubs *tkIntXlibStubsPtr;

  static int
  MyInitTkStubs (Tcl_Interp *ip)
  {
    if (Tcl_PkgRequireEx(ip, "Tk", "8.1", 0, (ClientData*) &tkStubsPtr) == NULL)      return 0;
    if (tkStubsPtr == NULL || tkStubsPtr->hooks == NULL) {
      Tcl_SetResult(ip, "This extension requires Tk stubs-support.", TCL_STATIC);
      return 0;
    }
    tkPlatStubsPtr = tkStubsPtr->hooks->tkPlatStubs;
    tkIntStubsPtr = tkStubsPtr->hooks->tkIntStubs;
    tkIntPlatStubsPtr = tkStubsPtr->hooks->tkIntPlatStubs;
    tkIntXlibStubsPtr = tkStubsPtr->hooks->tkIntXlibStubs;
    return 1;
  }
#endif
}
  }

    proc compiling {} {
        # check that we can indeed run a compiler
        # should only need to do this if we have to compile the code?
        if {[auto_execok [lindex $c::compile 0]] eq ""} {
            set v::compiling 0
        } else {
            set v::compiling 1
        }
        return $v::compiling
    }

    proc compiled {} {
        return [compiling]
    }

    proc done {} {
        return 0
    }

    proc failed {{silent ""}} {
        if {$v::libsrc == ""} {
            cbuild "" 0 "" $silent
        } else {
            lassign [cbuild $v::libsrc 0 ns silent] v::libfile v::ininame
        }
        proc failed {args} {
            puts stderr "error: critcl::failed can only be called once"
            exit 1
        }
        return $v::failed
    }

    proc tk {} {
        critcl::config tk 1
    }

    proc check {code} {
        variable run
        file mkdir $v::cache ;# just in case
        set pref [file normalize [file join $v::cache check_[pid]]]
        set src $pref.c
        set fd [open $src w]
        puts $fd $code
        close $fd
        set copts [list]
        if {$v::options(I) != ""} {
            lappend copts $c::include$v:::options(I)
        }
        lappend copts $c::include$v::cache
        set file [file normalize [info script]]
        # get the settings for this file into local variables
        set hdrs [append v::code($file,hdrs) ""] ;# make sure it exists
        set cmdline "$c::compile $v::cflags $copts $hdrs"
        set outfile $pref$c::object
        append cmdline " [subst $c::output] [file normalize $src]"
        if {[catch {interp eval $run exec $cmdline} err]} {
            set result 0
        } else {
            set result 1
        }
        foreach tmp [glob -directory $v::cache check_[pid].*] {
            file delete -force $tmp
        }
        return $result
    }

    proc crosscheck {} {
        variable targets
        global tcl_platform
        if {![catch {
            variable run
            set config [interp eval $run exec "$c::version 2>@stdout"]
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
            if {$host ne $target && [info exists targets($target)]} {
                setconfig $target
                puts stderr "Cross compiling using $target"
            }
        }
    }

    proc sharedlibext {} {
        return $c::sharedlibext
    }

    proc tsources {args} {
        lappend v::tsources $args
    }

    proc cdefines {defines {namespace "::"}} {
        foreach def $defines {
            lappend v::defines $namespace $def
        }
    }

    proc debug {args} {
        if {[lindex $args 0] eq "all"} {
            set args {memory symbols}
        }
        foreach arg $args {
            switch -- $arg {
                memory  { set v::cflags [concat $v::cflags $c::debug_memory] }
                symbols { set v::cflags [concat $v::cflags $c::debug_symbols]
                          set option::debug_symbols 1
                        }
                default {
                    puts stderr "error: unknown critcl::debug option - $arg"
                    exit 1
                }
            }
        }
    }

    proc build_defines {fd file} {
        # we process the cdefines in three steps
        #   - get the list of defines by preprocessing the source using the
        #     cpp -dM directive which causes any #defines to be output
        #   - extract the list of enums using regular expressions (not perfect,
        #     but will do for now)
        #   - generate Tcl_ObjSetVar2 commands to initialise Tcl variables
        set def [file normalize [file join $v::cache define_[pid]]]
        # first step - get list of matching defines
        set dfd [open $def.c w]
        puts $dfd $v::code(ccode)
        close $dfd
        set hdrs $v::code($file,hdrs)
        set cmd "$c::preproc_define $hdrs"
        set efd [open "| $cmd $def.c" r]
        set defines [list]
        while {[gets $efd line] >= 0} {
            set fields [split [string trim $line]]
            if {[lindex $fields 0] eq "#define"} {
                set var [lindex $fields 1]
                set val [lindex $fields 2]
                foreach {nm dfn} $v::defines {
                    if {[string match $dfn $var]} {
                        lappend defines $nm $var $val
                    }
                }
            }
        }
        # second step - get list of enums
        set cmd "$c::preproc_enum $hdrs"
        set efd [open "| $cmd $def.c" r]
        set code [read $efd]
        close $efd
        set matches [regexp -all -inline {enum [^\{\(\)]*{([^\}]*)}} $code]
        foreach {match submatch} $matches {
            foreach line [split $submatch \n] {
                foreach sub [split $line ,] {
                    set enum [lindex [split [string trim $sub]] 0]
                    foreach {nm dfn} $v::defines {
                        if {[string match $dfn $enum]} {
                            lappend defines $nm $enum $enum
                        }
                    }
                }
            }
        }
        # third step - generate Tcl_ObjSetVar2 commands
        foreach {nm df val} $defines {
            if {![info exists created($nm)]} {
                # we need to force the creation of the namespace
                # because this code will be run before the user code
                puts $fd "  Tcl_Eval(ip, \"namespace eval $nm {}\");"
                set created($nm) 1
            }
            set var "Tcl_NewStringObj(\"${nm}::$df\", -1)"
            if {$df eq $val} {
                # enum - assume integer
                set val "Tcl_NewIntObj($val)"
            } else {
                # text or int - force to string
                set val "Tcl_NewStringObj(\"$val\", -1)"
            }
            puts $fd "  Tcl_ObjSetVar2(ip, $var, NULL, $val, TCL_GLOBAL_ONLY);"
        }
        if {!$v::options(keepsrc)} { file delete $def.c }
        unset v::defines
    }

    proc clean_cache {} {
        foreach file [glob -nocomplain -directory $v::cache *] {
            file delete -force $file
        }
    }

    # read toolchain information from config file
    proc readconfig {config} {
        variable run
        variable configfile $config
        set cfg [open $config]
        set platforms [list]
        set cont ""
        set whenplat ""
        set platform $v::generic
        interp eval $run set platform $platform
        set i 0
        while {[gets $cfg line] >= 0} {
            incr i
            if {[set line [string trim $line]] ne ""} {
                # config lines can be continued using trailing backslash
                if {[string index $line end] == "\\"} {
                    append cont " [string range $line 0 end-1]"
                    continue
                }
                if {$cont ne ""} {
                    append cont $line
                    set line [string trim $cont]
                    set cont ""
                }
                set plat [lindex [split $line] 0]
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
                } elseif {$plat ne "#"} {
                    if {[lsearch -exact $v::configvars $plat] != -1} {
                        # default config option
                        set cmd ""
                        if {![regexp {(\S+)\s+(.*)} $line m type cmd]} {
                            # cmd is empty
                            set type $plat
                            set cmd ""
                        }
                        set plat ""
                    } else {
                        # platform config option
                        if {![regexp {(\S+)\s+(\S+)\s+(.*)} \
                                        $line m p type cmd]} {
                            # cmd is empty
                            set type [lindex $line 1]
                            set cmd ""
                        }
                        # if the target matches the current platform then we
                        # evaluate the when
                        if {$type eq "when" \
                            && ( [string match $platform* $plat] \
                                 || [string match $plat* $platform]
                                )} {
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
                        lappend platforms $plat
                    }
                    if {$type eq "target"} {
                        # cross compile target
                        if {$cmd eq ""} {
                            set cmd $plat
                        }
                        variable targets
                        set targets($plat) $cmd
                    } else {
                        set v::toolchain($plat,$type) $cmd
                    }
                }
            }
        }
        set platforms [lsort -unique $platforms]
        close $cfg
        if {$whenplat ne ""} {
            set match $whenplat
        } else {
            # try to match exactly
            set match [lsearch -exact -inline $platforms $platform]
            if {$match eq ""} {
                # try to match pattern
                foreach plat $platforms {
                    if {[string match $plat $platform]} {
                        set match $plat
                        break
                    }
                }
            }
        }
        setconfig ""    ;# defaults
        if {$match ne ""} {
            setconfig $match
        } else {
            setconfig $platform
        }
        set v::platforms $platforms
    }

    proc setconfig {plat} {
        variable run
        set v::config $plat
        set gen $v::generic
        set v::platform $plat
        # This should probably be in the Config someplace. We build with one
        # of win32-ix86-cl | win32-ix86-gcc but we produce win32-ix86
        if {[string match "win32-ix86-*" $plat]} {
           set v::platform "win32-ix86"
        }
        set c::platform ""
        set c::sharedlibext ""
        foreach var $v::configvars {
            if {[info exists v::toolchain($plat,$var)]} {
                set c::$var $v::toolchain($plat,$var)
                if {$var eq "platform"} {
                    set v::platform [lindex $c::platform 0]
                    set v::version [lindex $c::platform 1]
                }
            }
        }
        if {[info exists ::env(CFLAGS)]} {
            variable c::compile
            variable c::link
            append c::compile " $::env(CFLAGS)"
            append c::link " $::env(CFLAGS)"
            append c::link_preload " $::env(CFLAGS)"
        }
        if {[info exists ::env(LDFLAGS)]} {
            variable c::link
            append c::link " $::env(LDFLAGS)"
            append c::link_preload " $::env(LDFLAGS)"
        }
        if {[string match $v::platform $gen]} {
            # expand platform to match host if it contains wildcards
            set v::platform $gen
        }
        if {$c::platform eq ""} {
            # default config platform (mainly for the "show" command)
            set c::platform $plat
        }
        if {$c::sharedlibext eq ""} {
            set c::sharedlibext [info sharedlibextension]
        }
        cache [file join ~ .critcl $v::platform]
        #  set any Tcl variables Tcl variables
        foreach idx [array names v::toolchain $v::platform,*] {
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
    }

    proc optimize {} {
        set $option::optimize 1
    }

    proc showconfig {{fd ""}} {
        variable run
        set gen $v::generic
        if {$v::platform eq ""} {
            set plat "default"
        } else {
            set plat $v::platform
        }
        set out [list]
        if {$plat eq $gen} {
            lappend out "Config: $plat"
        } else {
            lappend out "Config: $plat (built on $gen)"
        }
        lappend out "    [format %-15s cache] [critcl::cache]"
        foreach var [lsort $v::configvars] {
            if {[catch {set val [interp eval $run [list subst [set c::$var]]]}]} {
                set val [set c::$var]
            }
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
        foreach idx [array names v::toolchain $v::platform,*] {
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

    # pre-load a shared library - will eventually be redundant when TIP #239
    # is widely available
    proc preload {lib} {
        if {[llength $v::preload] == 0} {
            file mkdir $v::cache
            file copy -force [file join $v::hdrdir preload.c] $v::cache
        }
        lappend v::preload $lib
    }

    proc license {who args} {
        if {[string trim $who] ne ""} {
            set license "This software is copyrighted by $who.\n"
        } else {
            set license ""
        }
        if {[llength $args]} {
            # use supplied license details
            append license [join $args]
        } else {
            set dir [file dirname [file dirname [file dirname $::sourcedir]]]
            set fd [open [file join $dir license.terms]]
            append license [join [lrange [split [read $fd] \n] 2 end] \n]
            close $fd
        }
        set libdir [file join $::libdir [file rootname $::outname]]
        file mkdir $libdir
        set fd [open [file join $libdir license.terms] w]
        puts $fd $license
        close $fd
    }

    proc showallconfig {{ofd ""}} {
        variable configfile
        set fd [open $configfile]
        set txt [read $fd]
        close $fd
        if {$ofd ne ""} {
            puts $ofd $txt
        } else {
            return $txt
        }
    }

    proc processargs {typesArray names cnames}  {
	upvar $typesArray types
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

    proc c++command {tclname class constructors methods} {
	# Build the body of the function to define a new tcl command for
	# the C++ class
	set helpline {}
	set classptr ptr_$tclname
	set comproc "    $class* $classptr;\n"
	append comproc "    switch (objc) \{\n"

	if {[llength $constructors] == 0} {
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
	    set nargs [llength $names]
     set ncargs [expr $nargs+2]
	   append comproc "        case $ncargs: \{\n"

	    if {$nargs == 0} {
		append comproc "            $classptr = new $class\();\n"
	    } else  {
		append comproc [processargs types $names $cnames]
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
	    if { $tmp == ""}  {
		lappend mnames  [lindex $n 0]
	    } else {
		lappend mnames [lindex $n 1]
	    }
	    lappend adefs $a
	}
	append cmdproc "    const char* cmds\[]=\{\"[join $tnames {","}]\",NULL\};\n"
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
	    set nargs [llength $names]
     set ncargs [expr $nargs+2]

	    append cmdproc "        case $ndx: \{\n"
	    append cmdproc "            if (objc==$ncargs) \{\n"
	    append cmdproc  [processargs types $names $cnames]
	    append cmdproc "                "
	    if {$rtype != "void"} {
		append cmdproc "$rtype2 rv = "
	    }
	    append cmdproc "$classptr->$mname\([join $cnames {, }]);\n"
	    append cmdproc "                "
	    switch -- $rtype {
	       void     { }
	       ok { append cmdproc "return rv;" }
	       int { append cmdproc "Tcl_SetIntObj(Tcl_GetObjResult(ip), rv);" }
	       long { append cmdproc " Tcl_SetLongObj(Tcl_GetObjResult(ip), rv);" }
	       float -
	       double { append cmdproc "Tcl_SetDoubleObj(Tcl_GetObjResult(ip), rv);" }
	       char* { append cmdproc "Tcl_SetResult(ip, rv, TCL_STATIC);" }
	       string -
	       dstring { append cmdproc "Tcl_SetResult(ip, rv, TCL_DYNAMIC);" }
	       vstring { append cmdproc "Tcl_SetResult(ip, rv, TCL_VOLATILE);" }
	       default  { append cmdproc "Tcl_SetObjResult(ip, rv); Tcl_DecrRefCount(rv);" }
	    }
	    append cmdproc "\n"
	    append cmdproc "                "
	    if {$rtype != "ok"} { append cmdproc "return TCL_OK;\n" }

	    append cmdproc "            \} else \{\n"
	    append cmdproc "               Tcl_WrongNumArgs(ip, 1, objv, \"$helpline\");\n"
	    append cmdproc "               return TCL_ERROR;\n"
	    append cmdproc "            \}\n"
	    append cmdproc "        \}\n"
	    incr ndx
	}
	    append cmdproc "    \}\n\}\n"

	critcl::ccode $delproc
	critcl::ccode $cmdproc
	# force ccommand to be in parent namespace (not in ::critcl)
	namespace eval [uplevel 1 namespace current] \
		[list critcl::ccommand $tclname {dummy ip objc objv} $comproc]
    }

    # read default configuration
    readconfig [file join [file dirname [info script]] Config]
}
