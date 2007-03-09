
#----------------------------------------------------------------------
#
# autoscroll.tcl --
#
#       Package to create scroll bars that automatically appear when
#       a window is too small to display its content.
#
#       Author: Kevin Kenny
#       See http://mini.net/tcl/950.html
#
#----------------------------------------------------------------------

package provide autoscroll 1.0

namespace eval autoscroll {
    namespace export autoscroll

    bind Autoscroll <Delete> [namespace code [list delete %W]]
    bind Autoscroll <Map> [namespace code [list map %W]]
}

#----------------------------------------------------------------------
#
# autoscroll::autoscroll --
#
#       Create a scroll bar that disappears when it is not needed, and
#       reappears when it is.
#
# Parameters:
#       w    -- Path name of the scroll bar, which should already
#               exist and have its geometry managed by the gridder.
#
# Results:
#       None.
#
# Side effects:
#       The widget command is renamed, so that the 'set' command can
#       be intercepted and determine whether the widget should appear.
#       In addition, the 'Autoscroll' bind tag is added to the widget,
#       so that the <Destroy> event can be intercepted.
#
# Notes:
#       It is an error to change the widget's gridding after
#       calling 'autoscroll' on it.
#
#----------------------------------------------------------------------

proc autoscroll::autoscroll { w } {

    variable grid
    variable needed

    rename $w [namespace current]::renamed$w

    proc ::$w {args} "
        return \[eval \[list autoscroll::widgetCommand $w\] \$args\]
    "

    set i [grid info $w]
    if { [string match {} $i] } {
        error "$w is not gridded"
    }
    set grid($w) $i
    set needed($w) 1

    bindtags $w [linsert [bindtags $w] 1 Autoscroll]

    eval [list ::$w set] [renamed$w get]

    return
}

#----------------------------------------------------------------------
#
# autoscroll::widgetCommand --
#
#       Widget command on an 'autoscroll' scrollbar
#
# Parameters:
#       w       -- Path name of the scroll bar
#       command -- Widget command being executed
#       args    -- Arguments to the commane
#
# Results:
#       Returns whatever the widget command returns
#
# Side effects:
#       Has whatever side effects the widget command has.  In
#       addition, the 'set' widget command is handled specially,
#       by setting/unsetting the 'needed' flag and gridding/ungridding
#       the scroll bar according to whether it is required.
#
#----------------------------------------------------------------------

proc autoscroll::widgetCommand { w command args } {

    variable grid
    variable needed

    switch -exact -- $command {
        set {
            foreach { min max } $args {}
            if { $min <= 0 && $max >= 1 } {
                if { [info exists needed($w)] } {
                    unset needed($w)
                    grid forget $w
                }
            } else {
                if { ! [info exists needed($w)] } {
                    set needed($w) {}
                    eval [list grid $w] $grid($w)
                }
            }
        }
    }

    return [eval [list renamed$w $command] $args]
}

#----------------------------------------------------------------------
#
# autoscroll::delete --
#
#       Delete an automatic scroll bar
#
# Parameters:
#       w -- Path name of the scroll bar
#
# Results:
#       None.
#
# Side effects:
#       Cleans up internal memory.
#
#----------------------------------------------------------------------

proc autoscroll::delete { w } {
    variable grid
    variable needed

    catch { unset grid($w) }
    catch { unset needed($w) }
    catch { rename renamed$w {} }

    return
}

#----------------------------------------------------------------------
#
# autoscroll::map --
#
#       Callback executed when an automatic scroll bar is mapped.
#
# Parameters:
#       w -- Path name of the scroll bar.
#
# Results:
#       None.
#
# Side effects:
#       Geometry of the scroll bar's top-level window is constrained.
#
# This procedure keeps the top-level window associated with an
# automatic scroll bar from being resized automatically after the
# scroll bar is mapped.  This effect avoids a potential endless loop
# in the case where the resize of the top-level window resizes the
# widget being scrolled, causing the scroll bar no longer to be needed.
#
#----------------------------------------------------------------------

proc autoscroll::map { w } {
    wm geometry [winfo toplevel $w] \
            [wm geometry [winfo toplevel $w]]
}
