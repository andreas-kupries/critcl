
package require emapint-both

ex encode  mix  ;# 1
ex xencode done ;# 2

ex decode  0 ;# init
ex xdecode 1 ;# mix

ex encode  foo
ex xencode bar
ex decode  55
ex decode  4
ex xdecode -2
ex xdecode 4
ex xencode MIX
