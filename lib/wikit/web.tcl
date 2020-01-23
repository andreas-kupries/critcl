# These routines are used when WiKit is called from CGI

package provide Web 1.0

package require cgi
package require Wikit::Db
package require Wikit::Search

# dump CGI env to file for debugging purposes
if {[catch {
  set logfd [open $env(WIKIT_DUMP) a]
  fconfigure $logfd -buffering line
  proc tclLog {msg} { puts $::logfd $msg }
  tclLog "#############################"
  foreach x [lsort [array names env]] {
    tclLog "\$$x = $env($x)"
  }
}]} {
  proc tclLog {msg} { }
}

tclLog ==============================

# 1-5-2001: new logic to work with a cache for *much* higher performance
#
# To make this work, create a "main" dir, reachable from the web, and store
# a special ".htaccess" file in it, (adjust as needed):
#	DirectoryIndex /home/jcw/wikit.cgi/0
#	ErrorDocument 404 /home/jcw/wikit.cgi
#
# Then config this wikit to maintain pages in that cache:
#	WIKI_CACHE=/home/jcw/www/tcl ./wikit.tkd
#
# Operation without this new env var, or with the CGI url remains unaffected.

if {[info exists env(WIKIT_CACHE)] && $env(WIKIT_CACHE) != ""} {
  set htmlcache $env(WIKIT_CACHE)
  tclLog "htmlcache = $htmlcache"
}

set EditInstructions {}
set ProtectedPages {}

# 3-5-2001: force graceful cleanup
proc cgi_mail_start {args} {
  catch {
    global _cgi env appname
    file delete $appname.lock
    set fd [open errors.txt a]
    puts $fd {=================================================================}
    puts $fd [clock format [clock seconds]]
    puts $fd $_cgi(errorInfo)
    puts $fd $env(PATH_INFO)
    close $fd
  }
  exit
}

rename puts jcw_puts
proc puts {args} {
  # 3-5-2001: make sure broken pipes won't abort
  catch {eval jcw_puts $args}
  if {[info exists ::htmlcopy]} {
    lassign $args a0 a1
    if {[llength $args] == 1} {
      append ::htmlcopy $a0 \n
    } elseif {[llength $args] == 2 && $a0 == "-nonewline"} {
      append ::htmlcopy $a1
    }
  }
  return
}

proc Wikit::stylesheet {} {
  global env
  if {[info exists env(WIKIT_CSS)]} {
    cgi_relationship stylesheet $env(WIKIT_CSS) type=text/css
  }
}

proc Wikit::ProcessCGI {} {
  variable readonly
  global htmlcopy htmlcache env roflag

  if {[info exists env(WIKIT_EMAIL)]} {
    admin_mail_addr $env(WIKIT_EMAIL)
  } else {
    admin_mail_addr nowhere@to.go
  }
  #debug -on

  # 2002-06-17: moved to app-wikit/start.tcl
  #input "n=1"
  suffix ""

  # support AOLserver, fix by Pascal Scheffers from http://mini.net/tcl/4416
  if { [string first "AOLserver/3" $env(SERVER_SOFTWARE)] > -1 } {
    #Aolserver does not provide a correct SCRIPT_NAME
    #fix this by parsing the WIKIT_BASE (which is mandatory anyway)
    catch {
      regexp -nocase {^http://[^/]+(/.+)/$} $env(WIKIT_BASE) - env(SCRIPT_NAME)
    }
  }

  set ::script_name $::env(SCRIPT_NAME)

  # fix embedded wikithttpd server script name
  if { [string first "WikitHttpd" $env(SERVER_SOFTWARE)] > -1 } {
      if { $::script_name == "/" } {
          set ::script_name ""
      }
  }

  # this code added 1-5-2001 to handle ErrorDocument redirection/caching
  if {[info exists ::env(REDIRECT_URL)]} {

    set r $env(REDIRECT_URL)
    if {[info exists env(PATH_INFO)]} { # DirectoryIndex case
      append r [string range $env(PATH_INFO) 1 end]
    } else {        # ErrorDocument case
      set env(PATH_INFO) /[file tail $r]
    }
    set env(SCRIPT_NAME) [file dirname $r]

      catch {set env(QUERY_STRING) $env(REDIRECT_QUERY_STRING)}

    if {[info exists htmlcache] && [regexp {\d+(\.html)?$} $r - x]} {
      tclLog "setting up cache copy - $r"
      set htmlcopy ""

      proc saveCopy {N} {
	global htmlcopy htmlcache env
	if {[info exists env(WIKIT_SAVE)] && $env(WIKIT_SAVE) eq "0"} return
	regsub ".*?\n\n" $htmlcopy "" htmlcopy
	if {$N == 2 || $htmlcopy == ""} return
	catch {
	  set fd [open [file join $htmlcache $N.html] w]
	  puts -nonewline $fd $htmlcopy
	  close $fd
	}
      }
    }
  }
  # end of new code

  cgi_eval {
    set host $::env(REMOTE_ADDR)
    #catch {set host $::env(REMOTE_HOST)}

    # this adds user name, if it is known (thx Shane McDonald, wiki page 19)
    catch {set host "$::env(REMOTE_USER)@$host"}

    set path ""
    catch {set path $::env(PATH_INFO)}

    set query ""
    catch {set query $::env(QUERY_STRING)}
    regsub {^Q=} $query {} query
    regsub {&.*} $query {} query

    set cmd ""
    set section ""
      # Updated 3Mar03, edit and references are now subdirs to allow
      # for site indexing.
      if {![regexp {^/(edit/|references/)?([0-9]+)(.*)$} $path x section N cmd] || $N >= [mk::view size wdb.pages]} {
      set N 0

        # try to locate a page by name, using various search heuristics
      if {[regexp {^/(.*)} $path x arg] && $arg != "" && $query == ""} {
        set N [mk::select wdb.pages name $arg -min date 1]
        switch [llength $N] {
          0 { # no match, try alternative approach
            # do a glob search, where AbCdEf -> *[Aa]b*[Cc]d*[Ee]f*
              # skip this if the search has brackets
            if {[string first \[ $arg] < 0} {
              regsub -all {[A-Z]} $arg \
                {*\\[&[string tolower &]\]} temp
              set temp "[subst -novariable $temp]*"
              set N [mk::select wdb.pages -glob name $temp -min date 1]
            }
            if {[llength $N] != 1} {
              set N 0
              set query $arg ;# turn it into a keyword search
            }
          }
          1 { # uniquely identified, done
          }
          default { # ambiguous, turn it into a keyword search
            set query $arg
          }
        }
      }
    }
    #tclLog "path $path query $query N $N"

    # prevent DoS via way too large integers
    if {$query eq "" && [string length $N] > 9} { set query $N }

    if {$query != ""} {
      set N 2
      variable searchKey
      variable searchLong
      set searchKey [unquote_input $query]
      set searchLong [regexp {^(.*)\*$} $searchKey x searchKey]
      set query "?$query"
    }

    pagevars $N name date who
    set origtag [list $date $who]
    set refs [mk::select wdb.refs to $N]

    # added 2004-05-17 wru cookie identification
    catch {source $::env(WIKIT_WRU)}

    # if there is new page content, save it now
    if {$N != "" && [lsearch -exact $::ProtectedPages $N] < 0} {
      if {$roflag < 0 && ![catch {import C}] && [import C] != ""} {
	# added 2002-06-13 - edit conflict detection
	if {![catch {import O}] && $O != $origtag} {
	  tclLog "conflict, want $O, stored $origtag"
	  http_head {
	    content_type
	    pragma no-cache
	  }
	  head {
	    cgi_http_equiv Content-type "text/html; charset=utf-8"
	    title $name
	    cgi_http_equiv Pragma no-cache
	    cgi_http_equiv Expire "Mon, 04 Dec 1999 21:29:02 GMT"
	    stylesheet
	  }
	  body {
	    h2 "Edit conflict on page $N - [Wiki $name $N]"
	    p "[bold {Your changes have NOT been saved}], because
	       someone (at IP address [lindex $origtag 1]) saved
	       a change to this page while you were editing."
	    p [italic {Please restart a new edit and merge your
	       version, which is shown in full below.}]
	    hr size=1
	    p "<pre>[quote_html $C]</pre>"
	    hr size=1
	    p
	  }
	  return
	}
	# thx Alistair Grant, see http://mini.net/tcl/9747
	#SavePage $N $C $host $name
	cgi_import_as Action editAction
	# Only actually save the page if the user selected "Save"
	if {[string trim $editAction] == "Save" && \
	    (![info exists ::env(WIKIT_WRU_REQ)] || [info exists wru_nick])} {
          # this cache flushing is not perfect - for one, it fails to be run
          # when running the same wiki in local mode as well, the solution is
          # to move this code to the modify layer, not just the cgi script use
          if {[info exists htmlcache]} {
            file delete $htmlcache/4.html
            file delete $htmlcache/$N.html
            # remove all referencing pages, if this page did not exist before
            # this makes sure that cache entries point to a filled-in page
            # from now on, instead of a "[...]" link to a first-time edit page
            if {$date == 0} {
              foreach r $refs {
                set r1 [mk::get wdb.refs!$r from]
                file delete $htmlcache/$r1.html
              }
            }
          }
	  SavePage $N $C $host $name
	  # end of change, 03-09-2003
	  mk::file commit wdb
	  set saved "saved"
	} else {
	  set saved "not saved"
	}
	# a general improvement: redirect through a fetch again
	if {![catch {import Z}]} {
	  tclLog "redirect $Z"
	  http_head {
	    content_type
	    pragma no-cache
	    #redirect $Z
	    #refresh 1 $Z
	  }
	  head {
	    title $name
	    http_equiv Refresh 1\;URL=$Z
	    stylesheet
	  }
	  body {
	    puts "Page $saved... [link - [Wikit::Format::quote $name] $Z]"
	  }
	  return
	}
	# end of changes
      }
    }

    # set up a few standard URLs an strings

    switch [llength $refs] {
      0 {
	set backRef ""
        set Refs ""
        set Title [Wikit::Format::quote $name]
      }
      1 {
        # 03-06-2003 generate page with back ref request, so cached
	# copy stays valid when more page references are added later
	#set backRef [mk::get wdb.refs!$refs from]
	set backRef references/$N!
        set Refs "[Wiki Reference $backRef] - "
        set Title [Wiki $name $backRef]
      }
      default {
	set backRef references/$N!
        set Refs "[llength $refs] [Wiki References $backRef] - "
        set Title [Wiki $name $backRef]
      }
    }

    set Edit "Edit [Wiki - edit/$N@]"

    if { $section eq "" } {
	set Home "Go to [Wiki - 0]"
	set About "About [Wiki - 1] - "
	set Search "[Wiki - 2] - "
	set Changes "[Wiki - 4] - "
	set Help " - [Wiki - 3]"
    } else {
	# either edit/ or references/ is in the URL. That means all
	# links must point to ../, to avoid confusion (it would still
	# work, though).
	set Home "Go to [Wiki - ../0]"
	set About "About [Wiki - ../1] - "
	set Search "[Wiki - ../2] - "
	set Changes "[Wiki - ../4] - "
	set Help " - [Wiki - ../3]"
    }

    if {$N == 1} { set About "" }
    if {$N == 2} { set Search "" }
    if {$N == 3} { set Help "" }
    if {$N == 4} { set Changes "" }

    if {$date != 0} {
      set date [clock format $date -gmt 1 -format {%e %b %Y, %R GMT}]
    }

    set updated "Updated [cgi_font size=-1 $date]"

    # added 2004-05-17
    if {[regexp {^(.+)[,@]} $who - who_nick] && $who_nick ne ""} {
      append updated " by $who_nick"
    }

    if {[lsearch -exact $::ProtectedPages $N] >= 0} {
      set menu ""
    } elseif {$roflag >= 0 || $readonly} {
      set menu "$updated[nl]"
    } else {
      set menu "$updated [nbspace]-[nbspace] $Edit"
      # 2004-05-29 add optional link to history
      if {[info exists ::env(WIKIT_REVS)] && $::env(WIKIT_REVS) ne ""} {
      append menu " [nbspace]-[nbspace]\
        [link - Revisions $::env(WIKIT_REVS)/$N]"
      }
      append menu [nl]
    }

    append menu "$Search$Changes$Refs$About$Home$Help"

    cgi_http_head {
      cgi_content_type
      pragma no-cache
    }

    # now dispatch on the type of request

    cgi_html {

      switch -- $cmd {

	@ { # called to generate an edit page
	  cgi_head {
	    cgi_http_equiv Content-type "text/html; charset=utf-8"
	    cgi_title "Edit $name"
	    cgi_meta name=robots content=noindex,nofollow
	    cgi_http_equiv Pragma no-cache
	    cgi_http_equiv Expire "Mon, 04 Dec 1999 21:29:02 GMT"
	    stylesheet
	    if {$N != "2" && [info exists ::env(WIKIT_BASE)]} {
	      cgi_base href=$::env(WIKIT_BASE)edit/
	    }
	  }

	  cgi_body bgcolor=#ffffff {
	    cgi_h2 [Wiki - ../$N]

	    cgi_form $::script_name/$N {
	      cgi_export O=$origtag
	      catch {
		set z "http://$::env(HTTP_HOST)$::env(REDIRECT_URL)"
		regsub {@$} $z {} z
		regsub /edit/ $z / z
		cgi_export Z=$z
	      }
	      textarea C=[GetPage $N] rows=30 cols=72 wrap=virtual \
		style=width:100%
	      p
	      # thx Alistair Grant, see http://mini.net/tcl/9747
	      #submit_button "=  Save  "
	      # Create Save and Cancel buttons
	      submit_button "Action=Save" \
	          [expr {[info exists ::env(WIKIT_WRU_REQ)] && \
	          ![info exists wru_nick] ? "disabled" : ""}]
	      cgi_puts " [nbspace] "
	      submit_button "Action=Cancel"
	      # end of change, 03-09-2003
	      # 2004-05-17 added some sugar to show wru identities
	      if {$date != 0} {
		cgi_puts " [nbspace] [nbspace] [nbspace] "
		cgi_puts [italic "Last saved on [bold $date]"]
		if {[info exists who_nick] && $who_nick ne ""} {
		  cgi_puts [italic " by [bold $who_nick]"]
		}
	      }
	      if {[info exists wru_nick]} {
		cgi_puts " [nbspace] (you are: [bold $wru_nick])"
	      } elseif {[info exists ::env(WIKIT_WRU_REQ)]} {
	        cgi_puts " [nbspace] ([bold [url Register $::env(WIKIT_WRU_REQ)]] to enable saving)"
	      }
	      p
	      cgi_puts $::EditInstructions
	    }
	  }
	}

	! { # called to generate a page with references
	  cgi_head {
	    cgi_http_equiv Content-type "text/html; charset=utf-8"
	    cgi_title "References to $name"
	    cgi_meta name=robots content=noindex,nofollow
	    cgi_http_equiv Pragma no-cache
	    cgi_http_equiv Expire "Mon, 04 Dec 1999 21:29:02 GMT"
	    stylesheet
	    if {$N != "2" && [info exists ::env(WIKIT_BASE)]} {
	      cgi_base href=$::env(WIKIT_BASE)references/
	    }
	  }

	  cgi_body bgcolor=#ffffff {
	    cgi_h2 "References to [Wiki - ../$N]"

	    set refList ""
	    foreach r $refs {
	      set r [mk::get wdb.refs!$r from]
	      pagevars $r name
	      lappend refList [list $name $r]
	    }

	    bullet_list {
	      # the items are a list, if we would just sort on them, then all
	      # single-item entries come first (the rest has {}'s around it)
	      # the following sorts again on 1st word, knowing sorts are stable
	      foreach x [lsort -dict -index 0 [lsort -dict $refList]] {
		lassign $x name r
		pagevars $r who date
		li "[GetTimeStamp $date] . . . [Wiki - ../$r] . . . $who"
	      }
	    }

	    hr noshade
	    cgi_puts [cgi_font size=-1 "$Search - $Changes - $About - $Home"]
	  }
	}

	default { # display one page, also handles expanded pages
	  cgi_head {
	    cgi_http_equiv Content-type "text/html; charset=utf-8"
	    #if {$N == 4} { cgi_http_equiv refresh 300 }
	    cgi_title $name
	    cgi_http_equiv Pragma no-cache
	    cgi_http_equiv Expire "Mon, 04 Dec 1999 21:29:02 GMT"
	    stylesheet
	    if {$N != "2" && [info exists ::env(WIKIT_BASE)]} {
	      cgi_base href=$::env(WIKIT_BASE)
	    }
	  }
	  cgi_body bgcolor=#ffffff {
	    set C [GetPage $N]
	    set U ""
	    foreach {C U} [Expand_HTML $C] break
	    set noTitle [regsub {^<p>(<img src=".*?")>} $C \
					[link - {\1 border=0>} $backRef] C]
	    if {!$noTitle} { h2 $Title }

	    if {$N == 2} {
	      # thx Alistair Grant, see http://mini.net/tcl/9748
	      isindex "prompt=Enter the search phrase. \
	      	Append an asterisk (*) to search page contents as well: "
	    }

	    p $C

	    hr noshade
	    #cgi_puts $menu
	    cgi_puts "<p id='footer'>$menu</p>"
	  }
	}
      }
    }

    if {[info exists ::htmlcopy]} { saveCopy $N }
  }
}
