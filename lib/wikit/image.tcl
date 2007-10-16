# Wikit Image handling
#
# LocalImages - clear/scan/get/list/local images

package provide Wikit::Image 1.0
package require Wikit::Format

namespace eval Wikit {
  namespace export LocalImages
  
  proc ScanForInlinedImages {{db wdb}} {
    array set inlined {}
    mk::loop c wdb.pages {
      set id [mk::cursor position c]
      pagevarsDB $db $id page
      set urls [StreamToUrls [TextToStream $page]]
      if {[llength $urls] > 0} {
        foreach {url type} $urls {
          if {$type eq "ref" && [regexp {\.(gif|jpg|png)$} $url]} {
            lappend inlined($url) $id
          }
        }
      }
    }
    array get inlined
  }
  
  proc LocalImages {cmd} {
    mk::view layout wdb.images {url date:I image:B}
    switch -- $cmd {
      clear {
        mk::view size wdb.images 0
      }
      scan {
        puts "Scanning [mk::view size wdb.pages] wiki pages..."
        array set scanned [ScanForInlinedImages]
        set dels 0
        set adds 0
        set obsolete {}
        mk::loop c wdb.images {
          set url [mk::get $c url]
          if {[info exists scanned($url)]} {
            unset scanned($url)
          } else {
            set obsolete [linsert $obsolete 0 $c]
            incr dels
          }
        }
        foreach x $obsolete { mk::row delete $x }
        foreach x [array names scanned] {
          mk::row append wdb.images url $x
          incr adds
        }
        puts "$adds urls added, $dels urls deleted"
      }
      list {
        puts "[mk::view size wdb.images] image cache entries:"
        foreach x [mk::select wdb.images -sort url] {
          set s [mk::get wdb.images!$x -size image]
          set d [clock format [mk::get wdb.images!$x date] -format "%Y/%m/%d"]
          if {$s == 0} {
            set d " (absent) "
            set s ""
          }
          puts [format {%10s  %s  %s} $s $d [mk::get wdb.images!$x url]]
        }
      }
      get {
        set todo [mk::select wdb.images date 0]
        puts "Fetching [llength $todo] images..."
        package require http
        package require autoproxy
        autoproxy::init
        set count 0
        foreach x $todo {
          array set f [mk::get wdb.images!$x]
          puts -nonewline "  $f(url) ... "
          flush stdout
          if {[catch {graburl $f(url)} y]} {
            puts "\n\t$y"
          } elseif {$y eq ""} {
            puts FAILED
          } elseif {[string length $y] > 102400} {
            puts "OMITTED > 100 Kb"
          } else {
            puts OK
            mk::set wdb.images!$x date [clock seconds] image $y
            if {[incr count] % 10 == 0} { mk::file commit wdb }
          }
        }
      }
      local {
        set n 0
        mk::loop c wdb.images {
          set url [mk::get $c url]
          set lfn [file tail $url]
          if {[file exists $lfn]} {
            set fd [open $lfn]
            fconfigure $fd -translation binary
            set data [read $fd]
            close $fd
            mk::set $c image $data date [file mtime $lfn]
            incr n
          }
        }
        puts "$n local images copied"
      }
      default {
        puts stderr "bad -images command, must be one of:\
 scan, list, get, local, or clear"
        exit 1
      }
    }
  }
  
}
