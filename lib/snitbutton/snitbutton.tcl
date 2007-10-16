 #
 #   snitbutton.tcl
 #
 #   Copyright (c) 2001-2002 by Steve Landers <steve (at) digital-smarties.com>
 #   Copyright (c) 2004 by Dr. Detlef Groth <detlef (at) dgroth.de>
 #                      porting to the snit framework
 #   Copyright (c) 2004 by Uwe Koloska <uwe (at) koloro.de>
 #                      removing the errors taken from the original
 #
 # Changes:
 # - width is now the width of the buttons
 # - the background of the standardbuttons is transparent
 #   and thus the background of the widget is now visible
 # - options are now interpreted if used as gButton
 # - sizes are now calculated the right way and honoured
 #
 # Bugs:
 # - there is no "#auto" and thus this package cannot replace gbutton in
 #   wikit, fixed this by adding a gButton namespace [DDG]

 package provide snitButton 0.4
 package provide gbutton 0.4
 package require Tk
 package require snit 0.93

 # this is a wrapper namespace for the gButton command, simply remove the gbutton dir and
 # add the snitbutton and the snit-dir to replace the itcl based gbutton.
 namespace eval gButton { }
 proc ::gButton {cmd path args} {
     set sb [eval snitButton $path.p $args]
     pack $sb -side left -fill x -expand no
     return $sb
 }
 proc gButton::modify {args} { eval ::snitButton::modify $args }
 proc gButton::cget {args} { eval ::snitButton::cget $args }
 proc gButton::init {args} { }

 snit::widget snitButton {
     variable canvas ""
     variable over
     variable numbut 0
     variable wid
     variable ht
     variable command

     option -padx 0
     option -pady 0
     option -font ""
     option -bg ""
     option -fill ""
     option -activefill ""
     option -disabledfill ""

     typevariable numobj 0
     typevariable textopts
     typevariable imageopts
     typevariable button
     typevariable up_img ""
     typevariable down_img ""
     typevariable disabled_img ""
     typevariable path ""

     constructor {args} {
 	#installhull $win
 	$self configurelist $args
 	set ht [expr {[image height $up_img] + 2*$options(-pady)}]
 	set wid [expr {[image width $up_img] + 2*$options(-padx)}]
 	set canvas [canvas $win.c$numobj -height $ht -width $wid \
 		        -highlightthickness 0]
 	if { $options(-bg) ne ""} {
 	    $canvas configure -background $options(-bg)
 	}
 	pack $canvas -padx 0 -pady 0
 	incr numobj
     }

     proc path {dir} {
 	set path $dir
     }

     proc init_opts {canv text} {
 	foreach arg [lsort [$canv itemconfigure img_$text]] {
 	    set imageopts([lindex $arg 0]) 1
 	}
 	foreach arg [lsort [$canv itemconfigure txt_$text]] {
 	    set textopts([lindex $arg 0]) 1
 	}
     }

     proc locate {text} {
 	return $button($text)
     }

     proc modify {text args} {
 	if {[info exists button($text)]} {
 	    #puts "$text  $args"
 	    eval $button($text) config $text $args
 	}
     }

     proc cget {text opt} {
 	if {[info exists button($text)]} {
 	    eval $button($text) get $text $opt
 	}
     }

     method new {text {cmd ""}} {
 	set x [expr {$numbut * $wid + $options(-padx)}]
 	set y $options(-pady)
 	set tag0 [$canvas create image $x $y -image $up_img -tag img_$text \
 		      -anchor nw]
 	$canvas bind $tag0 <ButtonPress-1> [list $self press $text down]
 	$canvas bind $tag0 <ButtonRelease-1> [list $self release $text]
 	set command($text) $cmd
 	set x [expr {$x + $wid/2 - $options(-padx)}]
 	set y [expr {$y + $ht/2 - $options(-pady)}]
 	set tag1 [$canvas create text $x $y -tag txt_$text -anchor center \
 		      -text $text]
 	$canvas bind $tag1 <ButtonPress-1> [list $self press $text down]
 	$canvas bind $tag1 <ButtonRelease-1> [list $self release $text]
 	if {$disabled_img != ""} {
 	    $canvas itemconfigure $tag0 -disabledimage $disabled_img
 	}
 	if {$options(-fill) != ""} {
 	    $canvas itemconfigure $tag1 -fill $options(-fill)
 	}
 	if {$options(-activefill) != ""} {
 	    $canvas itemconfigure $tag1 -activefill $options(-activefill)
 	}
 	if {$options(-disabledfill) != ""} {
 	    $canvas itemconfigure $tag1 -disabledfill $options(-disabledfill)
 	}
 	if {$options(-font) != ""} {
 	    $canvas itemconfigure $tag1 -font $options(-font)
 	}
 	set button($text) [list $self]
 	incr numbut
 	if {[array size textopts] == 0} {
 	    init_opts $canvas $text
 	}
 	$self size
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

     method release {text}  {
 	$self press $text up
 	# Do we need to make this "after idle", in case the command is
 	# long running?  Perhaps it is best done in the calling
 	# application if needed
 	uplevel #0 $command($text)
     }

     method size {} {
 	$canvas configure -width [expr {$numbut * $wid}]
     }
     typeconstructor {
 	set path [file dirname [info script]]
 	set up_img [image create photo -data {
 	    R0lGODlhRQAeAMYAAP////v7++zs7N/f39XV1dHR0c3NzcLCwrm5uby8vMfH
 	    x7S0tL29vcDAwMHBwbi4uLe3t8vLy8/Pz9DQ0Ojo6MbGxtjY2PLy8rOzs+Tk
 	    5Lu7u66urtPT0/b29q+vr6urq8PDw6SkpKampra2trq6upycnKCgoKWlpaen
 	    p6mpqZiYmJubm6GhoZeXl7+/v56enqKioqioqJ2dnZ+fn62traOjo6ysrJaW
 	    ltra2pGRkYyMjLGxsb6+vtTU1MrKysTExP//////////////////////////
 	    ////////////////////////////////////////////////////////////
 	    ////////////////////////////////////////////////////////////
 	    ////////////////////////////////////////////////////////////
 	    /////////////////////////////////////////////////yH5BAEKAB0A
 	    LAAAAABFAB4AAAf+gB2Cg4SFhoeIiYqLjI2Oj5CRkpOUlZaXiwIDBAUFBp+g
 	    oaKjpKWdBAMClQIFBwgJCa+ws7S1tre4CQw8Lgc9qpEDCgsICAwNDsnKy8zN
 	    zs/LDQ0MEQOQAwcPEA4REhPf4OHi4+Tl4RISPg4uCtaNFNkPFRMW9fb3+Pn6
 	    +/wTFewUGhXAgKGBBAL8EipcaI9AhGkFGGXQsAEDtwkcCHDYyLGjx48gQ24k
 	    QFLjhIcIXGRYVMDDBw8JHFSYSbOmzZs4c+qs8KPBA4IRFYH4EEKEBwwjHihd
 	    yrSp06dQoxKkkcKDAkUCSHwoYeLECRQoUoBNQbas2bNo06o1C/aECRP+H0gA
 	    O0RhhAgVK0ro3cu3r9+/gAP3XaEibkBEFEiwWNGisePHkCNLnkxZ8goWJA4f
 	    yuDiRYnKoEOLjlzihcpEGRTAMKGitevXsGPLnk1btgkYClYiLhBDxgzBwIML
 	    5ztDRowCmg1RIEDixQwTfn8Pny74+YwXJAgkL6RJAQ0VMNy+HU++vPnz6NN3
 	    haGCRru5mwu4qNGiBA0bMfLr38+/v////dlAw2c1uFCAboloYgAJMdygQgkw
 	    0EDQhBRWaOGFGE5IAwwlqHBDDCQYkAojFOCwIA013JCDDiy26OKLMMYoo4s5
 	    3FADDSHisB1qJoJAwg405FfDkEQWaeSRSCZTmR8NO5AAggE4IPjOAJ6A4AIJ
 	    JIyg5ZZcdunll2Bi6cKTBQywIyMCZLBJJ6W06eabpwyQAXyRCEBBBmoOoOee
 	    fPbp55+AypkBBXRiYuihiCb6SCAAOw==
 	}]
 	set disabled_img [image create photo -data {
 	    R0lGODlhRQAeAKUAAP////39/ff39/Hx8e3t7ezs7Orq6uXl5eLi4uPj4+Tk
 	    5Ofn5+Dg4Onp6eHh4evr6/X19e/v7/n5+d/f3/T09N3d3fv7+9zc3Obm5tnZ
 	    2dra2tvb29bW1tfX19TU1NXV1djY2NPT09HR0c/Pz97e3v//////////////
 	    ////////////////////////////////////////////////////////////
 	    /////////////////////////////////yH5BAEKAD8ALAAAAABFAB4AAAb+
 	    wJ9wSCwaj8ikcslsOp/QqHRKrVqvzkLBwO16v+CwWIv9FQ6IRCKtbrvf8Lhc
 	    rVAcqgsGIn3o+/+AgYKDggkNUgcODgcND46PkJGSk5STDQcKC1CJDgsPEaCh
 	    oqOkpaanDwuZWRMTBw8Ep7KztKEElwkFTQgVro0FBFrCw8TFxsfCBMrBD5cI
 	    CkwFFRcVCQcL2Nna29zd3t8LGImtukoYFxkavQyK7e7v8PHy8w6tFRsVmkoI
 	    FxwdGek0bNAgcIPBgwgTKlzI8CDBDB06XECwhIEGDx84aNzIsaPHjyBDdvzg
 	    YeISBCBIeljJsqXLlzBjynz5AQRFJQo0ztzJs+e+S43QlCwA0cGn0aMwO4DQ
 	    l6TABn8io0qdyrEDhw3l9vmz2pEr1a8hI1q9uWRBBQ8gIIpdy7at27dwxWYA
 	    4SFfkwIKMnjgMK2h37+AqXHwkEFB1iUGEGwIsRdEr1aQI0ueTLmyPRCDQ2xA
 	    YABK4goZQogYQbq06dOoU6s2LSJEhgqcpRjAgIDEvQ0Ac+vezbu37wwGK5BA
 	    gKHzlC0YFOxhwLy58+fQo0vfo6D4YSpaxGjfzp1Mme/gw4sfTx5LEAA7
 	}
 		         ]
 	set down_img [image create photo -data {
 	    R0lGODlhRQAeAMYAAP////n5+eXl5dLS0sXFxb+/v7i4uKWrsZGfsZGluIul
 	    xYufxZGlxZelv5+lsb/FxauxuIuXsZGly5Gry5ery5eluLG4uIufuIWfv6W4
 	    2au/2bG/2aW40pelxd/f34WfxZ+x0rjL3+zs7IuXq36Xv7jF37/L39nZ2Z+f
 	    q3eRuLHF36Wlq/Ly8oWRpXGLuHeRv5ex0ouXpaurq3eLq2SFsWuFuH6XxX6f
 	    xXGRv2uLuH6RsbGxsZ+fn1d3sV1+uGSFuGuFv2uLv5eXl1FxsVF3uFd3uGR+
 	    q0pxuKWlpWt3l1F3v0pxv3d+i1d+xUpxxYWFi2R3pVF+0kp30nF3hV13v1F+
 	    2Up32Up+2YuLi2RxhWR+v1eF2X5+fmRxd113pWSR312R31eL34WFhWtrcWRx
 	    fmR3l2uR0nGX38vLy2tra2RkZF1dXZGRkf//////////////////////////
 	    /////////////////////////////////////////////////yH5BAEKAH8A
 	    LAAAAABFAB4AAAf+gH+Cg4SFhoeIiYqLjI2Oj5CRkpOUlZaXiwIDBAUFBp+g
 	    oaKjpKWdBAMClQIFBwgJCgsKs7S1tre4uQoMDQ4HD6qRAxARCwsSExTKy8zN
 	    zs/QzBMTFRYDkAMHFxgUGRob4OHi4+Tl5uIaGhwUHRDXjR7aHyAbIfb3+Pn6
 	    +/z9GyDtPDASUWAEiQkaSoQwwbChw4cQI0qMGKJEBmoFRCw6gSIFiW4bVJRQ
 	    QbKkyZMoU6okWaLlyA0XF6w4sahACxcvFFAAwbOnz59AgwodCgLGhA8kYhRQ
 	    JELGDBo1Xti48aGq1atYs2rdytWGDRw5dOzQiEgADxc9fPz4AQRIkLb+QeLK
 	    nUu3rt27c9v+8OHDBY9ghzwIqTGESJHDiBMrXsy4sWPFRIb4FXhIhAceRogc
 	    2cy5s+fPoEOLBk3ECA8PZAuJOIEkiZIlsGPLnk27tu3btZUkQXIiNaHVO5g0
 	    cUK8uPHjyJMrX568CZMdvSt7KPAEShQp2LNr3869u/fv26NAeVIANSIPBHhM
 	    oVLFivv3V97Ln0+/vv36V6pQmcKDAOVDmuyARRZabFHFgQgmqOCCDDboYBVb
 	    aJEFFjukkshqBSDBRRdefAFGGCCGKOKIJJZoIolgfOFFF1wgUUB0iWhiAA9i
 	    jEFGGVqYccaOPPbo449ABrmjGVqUQcYYYvCXYICFTHmAxoxYcJGGGmtUaeWV
 	    WGap5ZZXqpEGF1goiYZ5i6z2pAw8sIGFGGJw4eabcMYp55x0sokFGzzIYAAa
 	    MA7kwQCeyIAEDzwIYeihiCaq6KKMEoqEngUMQKYjIghwwiadlKLpppyeMsAJ
 	    Avj2SKUenHDpAKimquqqrLbq6qcneBAqJiLUauutuOaq6665YuLrr4gEAgA7
 	}
 		     ]
     }

 }
