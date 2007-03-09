#
#   gbutton.tcl
#
#   Copyright (c) 2001-2002 by Steve Landers <steve@digital-smarties.com>
#
#   $Id$
#

if {[catch {package require Itcl}]} {
    set msg "Error: IncrTcl is required but isn't available\n\n"
    append msg "See http://www.equi4.com/tclkit for a version of TclKit "
    append msg "containing IncrTcl"
    catch {wm withdraw .}
    if {$tcl_platform(platform) == "unix"} {
        puts stderr $msg
    } elseif {[catch {package require Tk}]} {
        catch {console show}
        puts stderr $msg
    } else {
        wm withdraw .
        set t .itcl_missing
        catch {destroy $t}
        toplevel $t
        wm title $t "Error"
        wm protocol $t WM_DELETE_WINDOW "set ::itcl_error 1"
        label $t.l -relief flat -text $msg
        button $t.b -text "Exit" -command "set ::itcl_error 1"
        pack $t.l -fill x -pady 5 -padx 10
        pack $t.b
        vwait itcl_error
    }
} else {

  package provide gbutton 0.3
  package require Tk

  itcl::class gButton {

      private variable canvas ""
      private variable over
      private variable numbut 0
      private variable wid
      private variable ht
      private variable command

      public common padx 0
      public common pady 0
      public common font ""
      public common bg ""
      public common fill ""
      public common activefill ""
      public common disabledfill ""

      private common numobj 0
      private common textopts
      private common imageopts
      private common button
      private common up_img ""
      private common down_img ""
      private common disabled_img ""
      private common path [file dirname [info script]]

      constructor {frame args} {
          eval configure $args
          if {$up_img == ""} {
             gButton::init
          }
          set ht [expr {[image height $up_img] + $pady}]
          set wid [expr {[image width $up_img] + 2*$padx}]
          set canvas [canvas $frame.c$numobj -height $ht \
                  -highlightthickness 0]
          if {$bg != ""} {
            $canvas configure -background $bg
          }
          if {$font == ""} {
          }
          pack $canvas -padx 0 -pady 0
          incr numobj
      }

      proc path {dir} {
          set path $dir
      }

      proc init {args} {
          # used to initialise common variables - invoke using "-var value"
          foreach {opt val} $args {
              set [string range $opt 1 end] $val
          }
          if {[info exists up]} {
              set up_img [image create photo -file $up]
          } else {
              set up_img [image create photo -file [file join $path up.gif]]
          }
          if {[info exists down]} {
              set down_img [image create photo -file $down]
          } else {
              set down_img [image create photo \
                                -file [file join $path down.gif]]
          }
          if {[info exists disabled]} {
              set disabled_img [image create photo -file $disabled]
          } else {
              set disabled_img [image create photo \
                                    -file [file join $path disabled.gif]]
          }
      }

      private proc init_opts {canvas text} {
          foreach arg [lsort [$canvas itemconfigure img_$text]] {
              set imageopts([lindex $arg 0]) 1
          }
          foreach arg [lsort [$canvas itemconfigure txt_$text]] {
              set textopts([lindex $arg 0]) 1
          }
      }

      proc locate {text} {
          return $button($text)
      }

      proc modify {text args} {
          if {[info exists button($text)]} {
              eval $button($text) config $text $args
          }
      }

      proc cget {text opt} {
          if {[info exists button($text)]} {
              eval $button($text) get $text $opt
          }
      }

      method new {text {cmd ""}} {
          set x [expr {$numbut * $wid + $padx}]
          set y $pady 
          set tag0 [$canvas create image $x $y -image $up_img -tag img_$text \
                                  -anchor nw]
          $canvas bind $tag0 <ButtonPress-1> [list $this press $text down]
          $canvas bind $tag0 <ButtonRelease-1> [list $this release $text $tag0 %x %y]
          set command($text) $cmd
          set x [expr {$x + $wid/2 - $padx}]
          set y [expr {$y + $ht/2}]
          set tag1 [$canvas create text $x $y -tag txt_$text -anchor center \
                                -text $text]
          $canvas bind $tag1 <ButtonPress-1> [list $this press $text down]
          $canvas bind $tag1 <ButtonRelease-1> [list $this release $text $tag0 %x %y]
          if {$disabled_img != ""} {
              $canvas itemconfigure $tag0 -disabledimage $disabled_img
          }
          if {$fill != ""} {
              $canvas itemconfigure $tag1 -fill $fill
          }
          if {$activefill != ""} {
              $canvas itemconfigure $tag1 -activefill $activefill
          }
          if {$disabledfill != ""} {
              $canvas itemconfigure $tag1 -disabledfill $disabledfill
          }
          if {$font != ""} {
              $canvas itemconfigure $tag1 -font $font
          }
          set button($text) [list $this]
          incr numbut
          if {[array size textopts] == 0} {
              init_opts $canvas $text
          }
      }

      method config {text args} {
          foreach {opt arg} $args {
              if {$opt == "-command"} {
                  set command($text) $arg
              } else {
                  if {[info exists imageopts($opt)]} {
                      $canvas itemconfigure img_$text $opt $arg
                  }
                  if {[info exists textopts($opt)]} {
                      $canvas itemconfigure txt_$text $opt $arg 
                  }
              }
          }
      }

      method get {text opt} {
          set result ""
          if {[info exists textopts($opt)]} {
              set result [$canvas itemcget txt_$text $opt]
          } elseif {[info exists imageopts($opt)]} {
              set result [$canvas itemcget img_$text $opt]
          }
          return $result
      }

      method press {text event} {
          if {[string equal $event up]} {
              $canvas itemconfigure img_$text -image $up_img
          } else {
              $canvas itemconfigure img_$text -image $down_img
          }
      }

      method release {text id X Y}  {
          press $text up
          # Do we need to make this "after idle", in case the command is
          # long running?  Perhaps it is best done in the calling
          # application if needed
          lassign [$canvas bbox $id] x1 y1 x2 y2
          if {$x1 <= $X && $X <= $x2 && $y1 <= $Y && $Y <= $y2} {
              uplevel #0 $command($text)
          }
      }

      method size {} {
          $canvas configure -width [expr {$numbut * $wid}]
      }

  }
}
