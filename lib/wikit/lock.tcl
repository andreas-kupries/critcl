# Wikit locking
#
# AcquireLock - lock wikit by writing a lockfile
# ReleaseLock - unlock wikit by deleting lockfile

package provide Wikit::Lock 1.0

namespace eval Wikit {
  namespace export AcquireLock ReleaseLock

  proc AcquireLock {lockFile {maxAge 900}} {
    for {set i 0} {$i < 60} {incr i} {
      catch {
        set fd [open $lockFile]
        set opid [gets $fd]
        close $fd
        if {$opid != "" && ![file exists [file join / proc $opid]]} {
          file delete $lockFile
          # set fd [open savelog.txt a]
          # set now [clock format [clock seconds]]
          # puts $fd "# $now drop lock $opid -> [pid]"
          # close $fd
        }
      }
      catch {close $fd}

      if {![catch {open $lockFile {CREAT EXCL WRONLY}} fd]} {
        puts $fd [pid]
        close $fd
        return 1
      }
      after 1000
    }

    # if the file is older than maxAge, we grab the lock anyway
    if {[catch {file mtime $lockFile} t]} { return 0 }
    return [expr {[clock seconds] > $t + $maxAge}]
  }

  proc ReleaseLock {lockFile} {
    file delete $lockFile
  }
}
