# C Runtime In Tcl - compile C code on the fly

package provide critcl 0.35

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
  namespace export config csources clibraries cinit ccode ccommand cproc \
                cdata compiling scripting failed done tk cache tsources \
                platform cheaders cdefines

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

    # return a platform designator, including both OS and machine
    #
    # only use first element of $tcl_platform(os) - we don't care
    # whether we are on "Windows NT" or "Windows XP" or whatever
    #
    # transforms $tcl_platform(machine) for some special cases
    #  - on SunOS, matches for sun4* are transformed to sparc
    #  - on all OS's matches for intel and i*86* are transformed to x86
    #  - on MacOS X "Power Macintosh" is transformed to ppc
    #
    proc platform {} {
        global tcl_platform
        set plat [lindex $tcl_platform(os) 0]
        set mach $tcl_platform(machine)
        switch -glob -- $mach {
            sun4* { set mach sparc }
            intel -
            i*86* { set mach x86 }
            "Power Macintosh" { set mach ppc }
        }
	switch -- $plat {
	  AIX   { set mach ppc }
	  HP-UX { set mach hppa }
	}
        return "$plat-$mach"
    }

    proc cache {} {
        return [file join ~ .critcl [::critcl::platform]]
    }

  # keep all variables in a sub-namespace for easy access
  namespace eval v {
    variable cache	[::critcl::cache]
    variable hdrdir	[file join [file dirname [info script]] critcl_c]

    variable prefix	"v[package require critcl]"
    regsub {\.} $prefix {} prefix

    variable compile	{gcc -shared -DUSE_TCL_STUBS}
    # this should be deferred until after we know if we are cross compiling
    if {$::tcl_platform(platform) != "windows"} { lappend compile "-fPIC" }

    variable options
    array set options {outdir "" keepsrc 0 combine "" appinit "" force 0}
    array set options {I "" L "" tk 0 language "" lines 1}

    variable code	     ;# this array collects all code snippets
    variable curr	     ;# current digest
    variable compiling 0 ;# indicates that the gcc/cc is available
    variable failed 0    ;# set if compile fails
    variable ininame "" 
    variable libfile "" 
    variable sharedlibext [info sharedlibextension]
  }

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

  proc define {name args} {
    set v::curr [md5_hex "$name $args"]
    set file [file_normalize [info script]]

    set ns [uplevel 2 namespace current]
    if {$ns == "::"} { set ns "" } else { append ns :: }

    set ::auto_index($ns$name) [list [namespace current]::cbuild $file]
    #if {[info commands $name] != ""} { rename $name "" }
    
    lappend v::code($file,list) $name $v::curr
    set v::code($v::curr) "#define ns_$name \"$ns$name\"\n"
    if {$v::options(lines)} {
      append v::code($v::curr) "#line 1 \"[file tail $file]/$name\"\n"
    }
  }

  proc ccommand {name anames args} {
    define $name $anames $args

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
      emitln "tcl_$name$ca"
      emitln \{
      emit $body
      emitln \}
    } else {
      # if no body is specified, then $anames is alias for the real cmd proc
      emitln "#define tcl_$name $anames"
      emitln "int $anames\(\);"
    }
    unset v::curr
  }

  proc cproc {name adefs rtype {body "#"}} {
    define $name $adefs $rtype $body

    set cname c_$name
    set wname tcl_$name

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
      lappend cargs "$t $n"
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
	int - long - float - double - char* - Tcl_Obj* {
	  emitln "  $types($x) _$x;"
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
      int	{ emitln "  Tcl_SetIntObj(Tcl_GetObjResult(ip), rv);" }
      long	{ emitln "  Tcl_SetLongObj(Tcl_GetObjResult(ip), rv);" }
      float -
      double	{ emitln "  Tcl_SetDoubleObj(Tcl_GetObjResult(ip), rv);" }
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

  proc cbuild {{file ""} {load 1} {prefix {}} {silent ""}} {
    if {$file == ""} { set file [file_normalize [info script]] }

    # each unique set of cmds is compiled into a separate extension
    set digest [md5_hex "$file $v::code($file,list)"]

    set cache $v::cache
    regsub {^~} $cache "$::env(HOME)/" cache
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
    if {[lsearch -exact $hdrs "-g"] >= 0} { append libfile _g }

    # choose distinct suffix so switching between them causes a rebuild
    switch -- $v::options(combine) {
      ""         { append libfile $v::sharedlibext }
      dynamic    { append libfile _pic.o }
      static     { append libfile _stub.o }
      standalone { append libfile .o }
    }

    # the init proc name takes a capitalized prefix from the package name
    set ininame stdin ;# in case it's called interactively
    regexp {^\w+} [file tail $file] ininame
    set ininame [string totitle $ininame]
    if {$prefix != {}} {
        set ininame "${prefix}_$ininame"
    }

    if {$v::options(force) || ![file exists $libfile]} {
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
      # now do the Tk stuff
      if {$v::options(tk)} {
        setup_tk_stubs $fd
	if {![regexp { -DUSE_TK_STUBS\M} $v::compile]} {
	  append v::compile " -DUSE_TK_STUBS"
	}
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

      set copts [list]
      if {$v::options(language) != ""} {
	  lappend copts -x $v::options(language)
      }
      if {$v::options(I) != ""} {
        lappend copts -I$v:::options(I)
      }
      lappend copts -I$cache
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

      set cmdline "$v::compile $copts -o $libfile $base.c $srcs"
      if {$v::options(language) != ""} {
# Allow the compiler to determine the type of file
# otherwise it will try to compile the libs 
         append cmdline " -x none"
      }

      append cmdline " " $libs

      if {$v::options(combine) == ""} {
	if {![regexp { -g } $cmdline]} {
	  append cmdline " -O2 -DNDEBUG -Wl,-s"
	}
      } else {
        if {$::tcl_platform(os) eq "OSF1"} {
	  regsub { -shared } $cmdline { -c } cmdline
        } else {
	  regsub { -shared } $cmdline { -r -nostdlib } cmdline
        }
	if {$v::options(combine) != "dynamic"} {
	  regsub { -fPIC } $cmdline { } cmdline
	  if {$v::options(combine) == "standalone"} {
	    regsub { -DUSE_TCL_STUBS } $cmdline { } cmdline
	  }
	}
      }

      if {$::tcl_platform(os) == "Darwin"} {
	regsub { -shared } $cmdline { -dynamiclib -fno-common } cmdline
	regsub { -Wl,-s} $cmdline {} cmdline
      } elseif {$::tcl_platform(os) eq "OSF1"} {
	  regsub { -Wl,-s} $cmdline {} cmdline
      }

      puts $lfd $cmdline
      set v::failed 0
      if {[catch {
	  eval exec $cmdline 2>@ $lfd
	  if {!$v::options(keepsrc)} { file delete $base.c }
	  puts $lfd "$libfile: [file size $libfile] bytes"
      } err]} {
	  puts $lfd "ERROR while compiling code in $file:"
	  incr v::failed 
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
    } elseif {$load} { load $libfile $ininame }

    foreach {name digest} $v::code($file,list) {
      if {$name != "" && [info exists v::code($digest)]} {
	unset v::code($digest)
      }
    }
    foreach x {hdrs srcs init} {
      array unset v::code $file,$x
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
	set nul /dev/null
	if {$::tcl_platform(platform) == "windows"} {
	    set nul NUL
	}
	if {[catch {exec gcc -v 2> $nul}] && [catch {exec cc -v 2> $nul}]} {
            set v::compiling 0
        } else {
            set v::compiling 1
        }
        return $v::compiling
    }

    proc scripting {} {
        return [expr {$v::compiling == 0}]
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
        file mkdir $v::cache ;# just in case
        set pref [file normalize [file join $v::cache check_[pid]]]
        set src $pref.c
        set fd [open $src w]
        puts $fd $code
        close $fd
        set copts [list]
        if {$v::options(I) != ""} {
            lappend copts -I$v:::options(I)
        }
        lappend copts -I$v::cache
        set cmdline "$v::compile $copts [file normalize $src] -o $pref.o"
        if {[catch {eval exec $cmdline} err]} {
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
        global tcl_platform
	set platform ""
        if {![catch {set machine [eval exec "$v::compile -dumpmachine"]}]} {
            switch -glob -- $machine {
                *mingw* {
                    if {![string equal $tcl_platform(platform) windows]} {
                        set tcl_platform(byteOrder) littleEndian
                        set tcl_platform(machine) intel
                        set tcl_platform(os) "Windows NT"
                        set tcl_platform(osVersion) 5.0
                        set tcl_platform(platform) windows
                        set tcl_platform(wordSize) 4
                        set result 1
                        set v::sharedlibext .dll
                        set v::cache [::critcl::cache]
                        regsub { -fPIC} $v::compile {} v::compile
                        set platform Windows
                        set desc Xmingwin
                    }
                }
            }
        }
        if {$platform != ""} {
            puts stderr "Cross compiling for $platform using $desc"
        }
    }

    proc sharedlibext {} {
        return $v::sharedlibext
    }

    proc tsources {args} {
        lappend v::tsources $args
    }

    proc cdefines {defines {namespace "::"}} {
        foreach def $defines {
            lappend v::defines $namespace $def
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
        set cmd "$v::compile $v::code($file,hdrs) -E"
        set dfd [open $def.c w]
        puts $dfd $v::code(ccode)
        close $dfd
        set efd [open "| $cmd -dM $def.c" r]
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
        set efd [open "| $cmd $def.c" r]
        set code [read $efd]
        set matches [regexp -all -inline {enum [^\{\(\)]*{([^\}]*)}} $code]
        foreach {match submatch} $matches {
            foreach line [split $submatch \n] {
                foreach sub [split $line ,] {
                    set enum [lindex $sub 0]
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

}
