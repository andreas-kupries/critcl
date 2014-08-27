
package require emap_ex

puts ------------------------------

puts [encode  mix]  ;# 1
puts [xencode done] ;# 2

puts [decode  0] ;# init
puts [xdecode 1] ;# mix

puts ------------------------------

catch {
    encode foo
} msg ; puts foo:$msg

catch {
    xencode bar
} msg ; puts bar:$msg

catch {
    decode 55
} msg ; puts 55:$msg

catch {
    xdecode -2
} msg ; puts -2:$msg

puts ------------------------------

catch {
    xencode MIX
} msg ; puts MIX:$msg
