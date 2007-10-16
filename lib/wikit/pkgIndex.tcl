# Manually created, package setup

package ifneeded Wikit 1.1 [list source [file join $dir wikit.tcl]]

package ifneeded Wikit::Format 1.1 [list source [file join $dir format.tcl]]
package ifneeded Wikit::Gui 1.1 [list source [file join $dir gui.tcl]]

package ifneeded Wikit::Db 1.1 [list source [file join $dir db.tcl]]
package ifneeded Wikit::Cache 1.0 [list source [file join $dir cache.tcl]]
package ifneeded Wikit::Image 1.0 [list source [file join $dir image.tcl]]
package ifneeded Wikit::Lock 1.0 [list source [file join $dir lock.tcl]]
package ifneeded Wikit::Search 1.0 [list source [file join $dir search.tcl]]
package ifneeded Wikit::Utils 1.0 [list source [file join $dir utils.tcl]]

package ifneeded Web 1.0 [list source [file join $dir web.tcl]]
