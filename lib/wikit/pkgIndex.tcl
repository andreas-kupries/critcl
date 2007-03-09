# Manually created, package setup

package ifneeded Wikit 1.0 [list source [file join $dir wikit.tcl]]
package ifneeded Wikit::Format 1.0 [list source [file join $dir format.tcl]]
package ifneeded Wikit::Gui 1.0 [list source [file join $dir gui.tcl]]
package ifneeded Wikit::Utils 1.0 [list source [file join $dir utils.tcl]]

package ifneeded Modify 1.0 [list source [file join $dir modify.tcl]]
package ifneeded Web 1.0 [list source [file join $dir web.tcl]]
