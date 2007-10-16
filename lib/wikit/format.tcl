# Formatter for wiki markup text, CGI as well as GUI

package provide Wikit::Format 1.1

namespace eval Wikit::Format {
    namespace export TextToStream StreamToTk StreamToHTML StreamToRefs \
            StreamToUrls Expand_HTML
    
    # In this file:
    #
    # proc TextToStream {text} -> stream
    # proc StreamToTk {stream infoProc} -> {{tagged-text} {urls}}
    # proc StreamToHTML {stream cgiPrefix infoProc} -> {{html} {urls}}
    # proc StreamToRefs {stream infoProc} -> {pageNum ...}
    # proc StreamToUrls {stream} -> {url type ...}
    #
    # The "Text"   format is a Wiki-like one you can edit with a text editor.
    # The "Tk"     format can insert styled text information in a text widget.
    # The "HTML"   format is the format generated for display by a browser.
    # The "Refs"   format is a list with details about embedded references.
    # The "Urls"   format is a list of external references, bracketed or not.
    # The "Stream" format is a Tcl list, it's only used as interim format.
    
    # =========================================================================
    
    ### More format documentation
    
    # =========================================================================
    
    #
    # Ad "Tk")     This is a list of pairs {text taglist text taglist ...}
    #              which can be directly inserted into any text widget.
    #
    # Ad "Stream") This is the first time that the stream format is documented.
    #
    #     The base format is that of a list of pairs {cmd arg cmd arg ...}
    #     The available commands fall into three categories [x]:
    #
    #     1. Data carriers
    #     2. Visual markers
    #     3. Structural markers
    #
    #     [x] In the previous incarnation of this stream format the categories
    #         were essentially all mixed up and jumbled together. For example
    #         the command 'T' crossed category 1 and 3, introducing a new para-
    #         graph and also carrying the first fragment of text of said para-
    #         graph. That made for difficult creation and difficult interpreta-
    #         tion. It is the separation of the categories which makes the re-
    #         organized format much easier to generate and convert (<=> simpler
    #         code, which is even faster). (Not to mention the eviction of
    #         treating [, ], {, }, and \ as special characters. They are not).
    #
    #     Ad 1)   The empty string and 'g', 'u' and 'x'. The first is for text,
    #             the others specify the various possible links.
    #
    #             Cmd	Argument
    #             ------------------------------------------------------
    #             {}	The text to display
    #             g	Name/Title of referenced wiki page
    #             u	external URL, was unbracket'ed in sources
    #             x	external URL, bracket'ed in sources
    #             ------------------------------------------------------
    #
    #     Ad 2)   Currently only two: 'b' and 'i' for bold and italic emphasis.
    #             The argument specifies if the emphasis is switched on or off.
    #             The permitted values are 0 (off) and 1 (on).
    #
    #     Ad 3)   These are the markers for the various distinctive sections
    #             in wiki markup.
    #
    #             Cmd	'Begin' 			Argument
    #             ------------------------------------------------------
    #             T	Paragraph				Nesting level
    #             Q	Quoted line				Nesting level
    #             U	List item (unordered)	Nesting level
    #             O	List item (enumerated)	Nesting level
    #             I	List item (term)		Nesting level
    #             D	List item (term def)	Nesting level
    #             H	Horizontal rule			Line-width		
	#			  C code lines
    #             ------------------------------------------------------
    #
    #             Note: The current frontend renderer provides only nesting
    #                   level 0 and a line-width 1. The current backend
    #                   renderers ignore this information.
    #
    
    # =========================================================================
    # =========================================================================
    
    ### Frontend renderer                         :: Wiki Markup ==> Stream ###
    
    # =========================================================================
    # =========================================================================
    	
    ## Basic operation: Each line is classified via regexes and then handled
    ## according to its type. Text lines are coalesced into paragraphs, with
    ## some special code to deal with the boundary between normal text and
    ## verbatim quoted text. Each collected line is then separated into chunks
    ## of text, highlighting command and links (wiki page / external). This is
    ## then added to the internal representation.
    
    proc TextToStream {text {fixed 0} {code 0}} {
        # Based upon ideas from the kiwi renderer. One step rendering into
        # the internal representation without a script as intermediate step.
   
        set irep      [list]  ; # Internal representation generated here.
        set paragraph ""      ; # Buffer for the text of a single paragraph
        set empty_std 0       ; # boolean - set if the preceding line was empty
        set mode_fixed $fixed ; # flag to indicate currently in fixed font block
		set mode_code $code   ; # indicates code block (no markup)
		set mode_option 0	  ; # options (fixed option, variable description)
		set optnum 0	 	  ; # option block number
		set optlen 0	 	  ; # length of option block fixed part	
        foreach line [split $text \n] {
            # Per line, classify the it and extract the main textual information.
            foreach {tag depth txt aux} [linetype $line] break ; # lassign
            if {$mode_fixed && $tag ne "FIXED" && $tag ne "CODE" \
				 	&& $tag ne "EVAL"} {
				# if already in fixed mode, then line must be fixed 
				# or code content, or eval output
				set tag FIXED_CONTENT
			}
			if {$mode_option && $tag ne "OPTION"} {
				set tag OPTION_CONTENT
			}
            # Classification tags
            #
            # UL, OL, DL = Lists (unordered/bullet, ordered/enum,
            #                     definition/itemized)
            # PRE        = Verbatim / Quoted lines
            # HR         = Horizontal rule
            # STD        = Standard text
			# CODE		 = fixed font, no markup
			# FIXED		 = fixed font, markup
			# OPTION	 = start/end of option list
            
            ## Whenever we encounter a special line, not quoted, any
            ## preceding empty line has no further effect.
            #

            switch -exact -- $tag {
                HR - UL - OL - DL {set empty_std 0}
                default {}
            }
            
            ## Whenever we encounter a special line, including quoted, we
            ## have to render the data of the preceding paragraph, if
            ## there is any.
            #
            switch -exact -- $tag {
                HR - UL - OL - DL - PRE {
                    if {$paragraph != {}} {
                        lappend irep T 0 ; render $paragraph
                        set paragraph ""
                    }
                }
                default {}
            }
            
            ## Now processs the lines according to their types.
            #
            # Tag   | depth         | txt             | pfx           | aux
            # ------+---------------+-----------------+---------------+---------
            # UL    | nesting level | text of item    | before bullet | bullet
            # OL    | nesting level | text of item    | before bullet | bullet
            # DL    | nesting level | term definition | before bullet | term
            # PRE   | 1             | text to display |
            # HR    | 0             | text of ruler   |
            # STD   | 0             | text to display |
			# FIXED | 1			 	| text to display |	
			# CODE  | 1			 	| text to display |				
            # ------+---------------+-----------------+---------------+---------
            
            # HR     - Trivial
            # UL, OL - Mark their beginning and then render their text
            #        - like a normal paragraph.
            # DL     - Like list item, except that there are two different
            #          parts of text we have to render, term and term definition
            # PRE    - Quoted text is searched for links, but nothing
            #          more. An empty preceding line is added to the
            #          quoted section to keep it at a distance from the
            #          normal text coming before.
            # STD    - Lines are added to the paragraph until an empty one is
            #          encountered. This closes the paragraph.
			# CODE	 - fixed font - no markup
			# FIXED  - like CODE, but markup respected
            
            switch -exact -- $tag {
                HR  {lappend irep H 1}
                UL  {lappend irep U 0 ; render $txt}
                OL  {lappend irep O 0 ; render $txt}
                DL  {
                    lappend irep I 0 ; render $aux
                    lappend irep D 0 ; render $txt
                }
                PRE {
                    # Transform a preceding 'STD {}' into an empty Q line,
                    # i.e make it part of the verbatim section, enforce
                    # visual distance.
                    
                    if {$empty_std} {lappend irep Q 0 {} {}; set empty_std 0}
                    lappend irep Q 0
                    if {$txt != {}} {rlinks $txt}
                }
                STD {
                    if {$txt == {}} {
                        if {$paragraph != {}} {
                            lappend irep T 0 ; render $paragraph
                            set paragraph ""
                        }
                        set empty_std 1
                    } else {
                        if {$paragraph != {}} {append paragraph " "}
                        append paragraph $txt
                        set empty_std 0
                    }
                }
				CODE -
                FIXED {	
					if {$tag eq "CODE"} {
						set mode_code 1
					}
					if {$mode_fixed} {
						if {$paragraph ne {}} {
							set paragraph [join $paragraph \n]
							if {$mode_code} {
								lappend irep {} $paragraph
							} else {
								render $paragraph
							}
						}
						set mode_fixed 0
					} else {
						if {$paragraph ne ""} { 
							lappend irep T 0
							render $paragraph
						}
						set mode_fixed 1
						if {$empty_std} {
							lappend irep C 0 {} {}
							set empty_std 0
						}
						lappend irep C 0
					}
					set paragraph {}
				}
				FIXED_CONTENT {
					lappend paragraph $txt
				}
				OPTION {
					if {$mode_option} {
						# end option list and record max length of fixed part
						lappend irep L "$optnum $optlen"
						set mode_option 0
						set optlen 0
					} else {
						# start new option list
						if {$empty_std} { 
							lappend irep C 0 {} {}
							set empty_std 0 
						}
						if {$paragraph ne ""} {
							lappend irep T 0
							render $paragraph
							set paragraph ""
						}	                    
						set mode_option 1
						set optlen 0
						lappend irep L [incr optnum]
					}
				}
				OPTION_CONTENT {
					# the fixed part should be followed by one or more tabs
					# - if not then fall back to using the first double-space
					#   then the first space
					if {[regexp {^\s*(.+?)\t\s*(.*)$} $txt - opt rest] \
					 		|| [regexp {^\s*(.+?)\s{2,}(.*)$} $txt - opt rest] \
							|| [regexp {^\s*(.+?)\s+(.*)$} $txt - opt rest]} {
						set opt [string trim $opt]
						regsub -all \t $rest \s rest
						lappend irep F 0
						set optlen [max $optlen [render $opt]]
						lappend irep V \t
						render $rest V
					} elseif {$txt eq ""} {
						lappend irep F 0
					}
				}
				EVAL {
					if {![interp exists eval_interp]} {
						# create an intepreter to run eval commands
						# when running via web/ GGI this should be a safe interp
						interp create eval_interp
						# create the wikidir variable as a convenience
						set wikidir [file dirname $Wikit::wikifile]
 						set wikidir [file normalize $wikidir]
						eval_interp eval [list set wikidir $wikidir]

						# set auto_path in the interp to look for packages
						# in common places
							
						# lib directory next to dir containing the wiki
						set lib [file join [file dirname $wikidir] lib]
						if {[file isdirectory $lib]} {
							eval_interp eval [list lappend auto_path $lib]
						}
						
						# starkit.vfs/lib
						if {[info exists starkit::topdir]} {
							set lib [file join $starkit::topdir lib]
							if {[file isdirectory $lib]} {
								eval_interp eval [list lappend auto_path $lib]
							}
						}
					}
					# people might feel more comfortable putting quotes or
					# brackets around page references - so just strip them off
					set name [string trim $txt {'"[]}]
					set id [Wikit::LookupPage $name]
					set page [Wikit::GetPage $id]
					# delete any code markup in the page (this allows the
					# page to be displayed as code markup but still be run)
					regsub -all {(^======\n|\n======\n|\n======$)} $page {} page
					if {[catch {set txt [eval_interp eval $page]} msg]} {
						lappend irep i 1
						lappend irep "" "Error evaluating $txt:"
						lappend irep i 0
						lappend irep C 0
						lappend irep "" $msg
					} else {
						if {$mode_fixed} {
							if {$paragraph ne ""} {
								if {$mode_code} {
									lappend irep {} $paragraph
								} else {
									render $paragraph
								}
								set paragraph ""
							}
							lappend irep X 1
							set irep [concat $irep \
									[TextToStream $txt $mode_fixed $mode_code]]
							lappend irep X 0
						} else {
							append paragraph " $txt"
						}
					}
				}
                default {
                    error "Unknown linetype $tag"
                }
            }
        }
        
        # Render the last paragraph, if any.
        
        if {$paragraph != {}} {
			if {$mode_fixed} {
				set paragraph [join $paragraph \n]
				if {$mode_code} {
					lappend irep {} $paragraph
				} else {
					render $paragraph
				} 
			} else {
				lappend irep T 0
				render $paragraph
			}
        }
        return $irep
    }
    
	# returns the largest of two integers
	proc max {a b} {expr {$a > $b ? $a : $b}}
		
    proc linetype {line} {
        # Categorize a line of wiki text based on indentation and prefix
        
        set line [string trimright $line]
        
        ## Compat: retain tabs ...
        ## regsub -all "\t" $line "    " line
        #
        ## More compat'ibility ...
        ## The list tags allow non-multiples of 3 if the prefix contains at
        ## least 3 spaces. The standard wiki accepts anything beyond 3 spaces.
        ## Keep the kiwi regexes around for future enhancements.
        
        foreach {tag re} {
            UL	{^(   + {0,2})(\*) (\S.*)$}
            OL	{^(   + {0,2})(\d)\. (\S.*)$}
            DL	{^(   + {0,2})([^:]+):   (\S.*)$}
            
            UL	{^(   +)(\*) (\S.*)$}
            OL	{^(   +)(\d)\. (\S.*)$}
            DL	{^(   +)([^:]+):   (\S.*)$}
            
            FIXED  {^(===)$}
            CODE   {^(======)$}
			OPTION {^(\+\+\+)$}
			INDEX  {^(\+index)(\s?)(.+)$}
			EVAL {^(\+eval)(\s?)(.+)$}	
        } {
            # Compat: Remove restriction to multiples of 3 spaces.
            if {[regexp $re $line - pfx aux txt]} {
                #    && string length $pfx % 3 == 0
                return [list $tag [expr {[string length $pfx]/3}] $txt $aux]
            }
        }
        
        # PO	{^\+-\S+([^\S]+)\S+(\S.*)$}
        
        # Compat: Accept a leading TAB is marker for quoted text too.
        
        if {([string index $line 0] == " ") || ([string index $line 0] == "\t")} {
            return [list PRE 1 $line]
        }
        if {[regexp {^-{4,}$} $line]} {
            return [list HR 0 $line]
        }
        return [list STD 0 $line]
    }
    
    proc rlinks {text} {
        # Convert everything which looks like a link into a link. This
        # command is called for quoted lines, and only quoted lines.
        
        upvar irep irep
        
	# Compat: (Bugfix) Added " to the regexp as proper boundary of an url.
        set re {\m(https?|ftp|news|mailto|file):(\S+[^\]\)\s\.,!\?;:'>"])}
        set txt 0
        set end [string length $text]
        
        ## Find the places where an url is inside of the quoted text.
        
        foreach {match dummy dummy} [regexp -all -indices -inline $re $text] {
            # Skip the inner matches of the RE.
            foreach {a e} $match break
            if {$a > $txt} {
                # Render text which was before the url
                lappend irep {} [string range $text $txt [expr {$a - 1}]]
            }
            # Render the url
            lappend irep u [string range $text $a $e]
            set txt [incr e]
        }
        if {$txt < $end} {
            # Render text after the last url
            lappend irep {} [string range $text $txt end]
        }
        return
    }
    
    proc render {text {mode ""}} {
        # Rendering of regular text: links, markup, brackets.
        
        # The main idea/concept behind the code below is to find the
        # special features in the text and to isolate them from the normal
        # text through special markers (\0\1...\0). As none of the regular
        # expressions will match across these markers later passes
        # preserve the results of the preceding passes. At the end the
        # string is split at the markers and then forms the list to add to
        # the internal representation. This way of doing things keeps the
        # difficult stuff at the C-level and avoids to have to repeatedly
        # match and process parts of the string.
        
        upvar irep irep
        variable codemap
        
        ## puts stderr \]>>$irep<<\[
        ## puts stderr >>>$text<<<
        
        # Detect page references, external links, bracketed external
        # links, brackets and markup (hilites).
        
        # Complex RE's used to process the string
        set pre  {\[([^\]]*)]}  ; # page references ; # compat
		set lre  {\m(https?|ftp|news|mailto|file):(\S+[^\]\)\s\.,!\?;:'>"])} ; # links
		set blre "\\\[\0\1u\2(\[^\0\]*)\0\\\]"

        # " - correct emacs hilite
        
        # Order of operation:
        # - Remap double brackets to avoid their interference.
        # - Detect embedded links to external locations.
        # - Detect brackets links to external locations (This uses the
        #   fact that such links are already specially marked to make it
        #   easier.
        # - Detect references to other wiki pages.
        # - Render bold and italic markup.
        #
        # Wiki pages are done last because there is a little conflict in
        # the RE's for links and pages: Both allow usage of the colon (:).
        # Doing pages first would render links to external locations
        # incorrectly.
        #
        # Note: The kiwi renderer had the order reversed, but also
        # disallowed colon in page titles. Which is in conflict with
        # existing wiki pages which already use that character in titles
        # (f.e. [COMPANY: Oracle].
        
        # Make sure that double brackets do not interfere with the
        # detection of links.
        regsub -all {\[\[} $text {\&!} text
        
        ## puts stderr A>>$text<<*
        
        # Isolate external links.
        regsub -all $lre $text "\0\1u\2\\1:\\2\0" text
        ## puts stderr C>>$text<<*
        
        # External links in brackets are simpler cause we know where the
        # links are already.
        regsub -all $blre $text "\0\1x\2\\1\0" text
        ## puts stderr D>>$text<<*
        
        # Now handle wiki page references
        regsub -all $pre $text "\0\1g\2\\1\0" text
        ## puts stderr B>>$text<<*
        
        # Hilites are transformed into on and off directives.
        # This is a bit more complicated ... Hilites can be written
        # together and possible nested once, so it has make sure that
        # it recognizes everything in the correct order!
        
        # Examples ...
        # {''italic'''''bold'''}         {} {<i>italic</i><b>bold</b>}
        # {'''bold'''''italic''}         {} {<b>bold</b><i>italic</i>}
        # {'''''italic_bold'''''}        {} {<b><i>italic_bold</i></b>}
		# {`fixed`}						 {} {... to be added ...}
        
        # First get all un-nested hilites
        while {
            [regsub -all {'''([^']+?)'''} $text "\0\1b+\0\\1\0\1b-\0" text] ||
            [regsub -all {''([^']+?)''}   $text "\0\1i+\0\\1\0\1i-\0" text] ||
			[regsub -all {`(.+?)`}   $text "\0\1f+\0\\1\0\1f-\0" text]
        } {}
        
        # And then the remaining ones. This also captures the hilites
        # where the highlighted text contains single apostrophes.
        
        regsub -all {'''(.+?)'''} $text "\0\1b+\0\\1\0\1b-\0" text
        regsub -all {''(.+?)''}   $text "\0\1i+\0\\1\0\1i-\0" text
        
        
        # Normalize brackets ...
        set text [string map {&! [ ]] ]} $text]
        
        # Listify and generate the final representation of the paragraph.
        
        ## puts stderr *>>$text<<*
        
		set len 0
        foreach item [split $text \0] {
            ## puts stderr ====>>$item<<<
            
            set cmd {} ; set detail {}
            foreach {cmd detail} [split $item \2] break
            set cmd [string trimleft $cmd \1]
            
            ## puts stderr ====>>$cmd|$detail<<<
            
            switch -exact -- $cmd {
                b+    {lappend irep b 1}
                b-    {lappend irep b 0}
                i+    {lappend irep i 1}
                i-    {lappend irep i 0}
				f+    {lappend irep f 1}
				f-    {lappend irep f 0}
                default {
                    if {$detail == {}} {
                        # Pure text
                        if {$cmd != ""} {
                            lappend irep $mode $cmd
							incr len [string length $cmd]
                        }
                    } else {
                        # References.
                        #2003-06-20: remove whitespace clutter in page titles
                        regsub -all {\s+} [string trim $detail] { } detail
                        lappend irep $cmd $detail
						incr len [string length $detail]
                    }
                }
            }
            
            ## puts stderr ======\]>>$irep<<\[
        }
        ## puts stderr ======\]>>$irep<<\[
        return $len
    }
    
    # =========================================================================
    # =========================================================================
    
    ### Backend renderer                                   :: Stream ==> Tk ###
    
    # =========================================================================
    # =========================================================================
    
    # Output specific conversion. Takes a token stream and converts this into
    # a three-element list. The first element is a list of text fragments and
    # tag-lists, as described at the beginning as the "Tk" format. The second
    # element is a list of triples listing the references found in the page.
    # This second list is required because some information about references
    # is missing from the "Tk" format. And adding them into that format would
    # make the insertion of data into the final text widget ... complex (which
    # is an understatement IMHO). Each triple consists of: url-type (g, u, x),
    # page-local numeric id of url (required for and used in tags) and
    # reference text, in this order.  The third list is a list of embedded
    # images (i.e. stored in "images" view), to be displayed in text widget.
    
    # Note: The first incarnation of the rewrite to adapt to the new
    # "Stream" format had considerable complexity in the part
    # assembling the output. It kept knowledge about the last used
    # tags and text around, using this to merge runs of text having
    # the same taglist, thus keeping the list turned over to the text
    # widget shorter. Thinking about this I came to the conclusion
    # that removal of this complexity and replacing it with simply
    # unconditional lappend's would gain me time in StreamToTk, but
    # was also unsure how much of a negative effect the generated
    # longer list would have on the remainder of the conversion (setup
    # of link tag behaviour in the text widget, insertion in to the
    # text widget). Especially if the drain would outweigh the gain.
    # As can be seen from the code chosen here, below, I found that
    # the gain through the simplification was much more than the drain
    # later. I gained 0.3 usecs in this stage and lost 0.13 in the
    # next (nearly double time), overall gain 0.17.
    
    proc StreamToTk {s {ip ""}} {
        variable tagmap ; # pre-assembled information, tags and spacing
        variable vspace ; # ....
        #             ; # State of renderer
        set urls   "" ; # List of links found
        set eims   "" ; # List of embedded images
        set result "" ; # Tk result
        set state  T  ; # Assume a virtual paragraph in front of the actual data
        set count  0  ; # Id counter for page references
        set xcount 0  ; # Id counter for bracketed external references
        set number 0  ; # Counter for items in enumerated lists
        set b      0  ; # State of bold emphasis    - 0 = off, 1 = on
        set i      0  ; # State of italic emphasis  - 0 = off, 1 = on
		set f      0  ; # State of fixed-width font - 0= off, 1 = on
		set incl   0  ; # included file? (0 = no, 1 = yes)

        foreach {mode text} $s {
            switch -exact -- $mode {
                {} {
                    if {$text == {}} {continue}
					if {$incl && $state eq "C"} {
						# prepend three spaces to each line of included file
						set new [join [split $text \n] "\n   "]
						if {$new ne $text} {
							set text "   $new"
						}
					}
                    lappend result $text $tagmap($state$b$i)
                }
                b - i {set $mode $text ; # text in {0,1}}
                g {
                    set     n    [incr count]
                    lappend urls g $n $text
                    set     tags [set base $tagmap($state$b$i)]
                    lappend tags url g$n
                    
                    if {$ip == ""} {
                        lappend result $text $tags
                        continue
                    }
                    
                    set info [lindex [eval $ip [list $text]] 2]
                    
                    if {$info == "" || $info == 0} {
                        lappend result \[ $tags $text $base \] $tags
                        continue
                    }
                    
                    lappend result $text $tags
                }
                u {
                    set n [incr count]
                    lappend urls u $n $text
                    
                    set tags $tagmap($state$b$i)
                    if {[lindex $tags 0] == "fixed"} {
                        lappend tags urlq u$n
                    } else {
                        lappend tags url u$n
                    }
                    
                    lappend result $text $tags
                }
                x {
                    # support embedded images if present in "images" view
                    set iseq ""
                    if {[regexp {\.(gif|jpg|png)$} $text - ifmt]} {
                        set iseq [mk::select wdb.images url $text -count 1]
                        if {$iseq != "" && [info commands eim_$iseq] == ""} {
                            if {$ifmt == "jpg"} { set ifmt jpeg }
                            catch { package require tkimg::$ifmt }
                            catch {
                                image create photo eim_$iseq -format $ifmt \
                                        -data [mk::get wdb.images!$iseq image]
                            }
                        }
                    }
                    if {[info commands eim_$iseq] != ""} {
                        #puts "-> $xcount $text"
                        lappend result " " eim_$iseq
                        lappend eims eim_$iseq
                    } else {
                        set n [incr xcount]
                        lappend urls x $n $text
                        
                        set     tags [set base $tagmap($state$b$i)]
                        lappend tags url x$n
                        lappend result \[ $base $n $tags \] $base
                    }
                }
                Q {
                    set number 0 ;# reset counter for items in enumerated lists
                    # use the body tag for the space before a quoted string
                    # so the don't get a gray background.
                    lappend result $vspace($state$mode) $tagmap(T00)
                    set state $mode
                }
                T - I - D - C {
                    set number 0 ;# reset counter for items in enumerated lists
                    lappend result $vspace($state$mode) $tagmap(${mode}00)
                    set state $mode
                }
                U {
                    lappend result \
                            "$vspace($state$mode)   \u2022  " $tagmap(${mode}00)
                    set state $mode
                }
                O {
                    lappend result \
	                    "$vspace($state$mode)   [incr number].\t" \
	 					$tagmap(${mode}00)
                    set state $mode
                }
                H {
                    lappend result \
                            $vspace($state$mode) $tagmap(T00) \
                            \t                   $tagmap(Hxx) \
                            \n                   $tagmap(H00)
                    set state $mode
                }
				L {	# start/end of option list
					set text [split $text]
					set optnum [lindex $text 0]
					if {[set len [lindex $text 1]] ne ""} {
						# end - set width of fixed part of option block
						Wikit::optwid $optnum $len
						set state T
					}
				}
				F { # fixed text part of option declaration
					set indent "   "
					lappend result $vspace(TF)$indent $tagmap(F00)$optnum
					set state F
				}
				V { # variable text part of option declaration
					set tag $tagmap(V$b$i)
					set font [lindex $tag 0]$optnum
					set attr [lrange $tag 1 end]
					lappend result $text "$font $attr"
				}
				X {
					set incl $text
				}
				f {
					if {$text} {
						set oldstate $state
						set state Y
					} else {
						set state $oldstate
					}
				}
            }
        }
        list [lappend result "" body] $urls $eims
    }
    
    # Map from the tagcodes used in StreamToTk above to the taglist
    # used in the text widget the generated text will be inserted into.
    
    variable  tagmap
    array set tagmap {
        T00 body     T01 {body i}    T10 {body b}    T11 {body bi}
        Q00 fixed    Q01 {fixed i}   Q10 {fixed b}   Q11 {fixed bi}
        H00 thin     H01 {thin i}    H10 {thin b}    H11 {thin bi}
        U00 ul       U01 {ul i}      U10 {ul b}      U11 {ul bi}
        O00 ol       O01 {ol i}      O10 {ol b}      O11 {ol bi}
        I00 dt       I01 {dt i}      I10 {dt b}      I11 {dt bi}
        D00 dl       D01 {dl i}      D10 {dl b}      D11 {dl bi}
    	C00 code     C01 {code fi}   C10 {code fb}   C11 {code fbi}
        V00 {optvar} V01 {optvar vi} V10 {optvar vb} V11 {optvar vbi}
        F00 {optfix} F01 {optfix fi} F10 {optfix fb} F11 {optfix fbi}
        Y00 fwrap    Y01 {fwrap i}   Y10 {fwrap b}   Y11 {fwrap bi}
        Hxx {hr thin}

    }
    
    # Define amount of vertical space used between each logical section of text.
    #			| Current              (. <=> 1)
    #  Last		| T  Q  U  O  I  D  H  C  X  Y 
    # ----------+-----------------------------
    #  Text   T | 2  2  2  2  2  1  2  1  0  0
    #  Quote  Q | 2  1  2  2  2  1  3  1  0  0
    #  Bullet U | 2  2  1  1  1  1  2  1  0  0
    #  Enum   O | 2  2  1  1  1  1  2  1  0  0
    #  Term   I | 2  2  1  1  1  1  2  1  0  0
    #  T/def  D | 2  2  1  1  1  1  2  1  0  0
    #  HRULE  H | 1  1  1  1  1  1  2  1  1  1
	#  CODE   C | 2  2  2  2  2  1  3  1  0  0
	#  INCL   X | 0  0  0  0  0  0  2  0  0  0
	#  fixed  Y | 0  0  0  0  0  0  2  0  0  0
    # ----------+-----------------------------
    
    variable  vspace
    proc vs {last current dummy n} {
        variable vspace
        set      vspace($last$current) [string repeat \n $n]
        return
    }

    vs T T --- 2 ; vs T Q --- 2 ; vs T U --- 2 ; vs T O --- 2 ; vs T I --- 2
    vs Q T --- 2 ; vs Q Q --- 1 ; vs Q U --- 2 ; vs Q O --- 2 ; vs Q I --- 2
    vs U T --- 2 ; vs U Q --- 2 ; vs U U --- 1 ; vs U O --- 1 ; vs U I --- 1
    vs O T --- 2 ; vs O Q --- 2 ; vs O U --- 1 ; vs O O --- 1 ; vs O I --- 1
    vs I T --- 2 ; vs I Q --- 2 ; vs I U --- 1 ; vs I O --- 1 ; vs I I --- 1
    vs D T --- 2 ; vs D Q --- 2 ; vs D U --- 1 ; vs D O --- 1 ; vs D I --- 1
    vs H T --- 1 ; vs H Q --- 1 ; vs H U --- 1 ; vs H O --- 1 ; vs H I --- 1
  
	vs T D --- 1 ; vs T H --- 2 	
	vs Q D --- 1 ; vs Q H --- 3
	vs U D --- 1 ; vs U H --- 2
	vs O D --- 1 ; vs O H --- 2 
	vs I D --- 1 ; vs I H --- 2 
	vs D D --- 1 ; vs D H --- 2
	vs H D --- 1 ; vs H H --- 2

    # support for fixed font / code blocks
	vs T C --- 1 ; vs Q C --- 1 ; vs U C --- 1 ; vs O C --- 1 ; vs I C --- 1
    vs D C --- 1 ; vs H C --- 1

    vs C T --- 2 ; vs C Q --- 2 ; vs C U --- 2 ; vs C O --- 2 ; vs C I --- 2
	vs C D --- 1 ; vs C H --- 3 ; vs C C --- 1 ; vs C X --- 0 ; vs C Y --- 0

	# support for options
	vs L F --- 0 ; vs F V -- 0 ; vs V T --- 1 ; vs T F --- 1 ; vs L T --- 1
	vs F T --- 1
	
	# support for included files/evals
	vs X T --- 0 ; vs X Q --- 0 ; vs X U --- 0 ; vs X O --- 0 ; vs X I --- 0
	vs X D --- 0 ; vs X H --- 1 ; vs X C --- 0 ; vs X X --- 0 ; vs X Y --- 0
	
	# fixed font
	vs Y T --- 0 ; vs Y Q --- 0 ; vs Y U --- 0 ; vs Y O --- 0 ; vs Y I --- 0
	vs Y D --- 0 ; vs Y H --- 1 ; vs Y C --- 0 ; vs Y X --- 0 ; vs Y I --- 0
	
  	rename vs {}
    
    # =========================================================================
    # =========================================================================
    
    ### Backend renderer                                 :: Stream ==> HTML ###
    
    # =========================================================================
    
    # expand a page string to HTML
    proc Expand_HTML {str {db wdb}} {
        StreamToHTML [TextToStream $str] $::env(SCRIPT_NAME) \
					 [list ::Wikit::InfoProc $db]
    }
    
    # =========================================================================
    
    # Output specific conversion. Takes a token stream and converts this
    # into HTML. The result is a 2-element list. The first element is the
    # HTML to render. The second element is alist of triplets listing all
    # references found in the stream (each triplet consists reference
    # type, page-local numeric id and reference text).
    
    proc StreamToHTML {s {cgi ""} {ip ""}} {
        set result ""
        set state H   ; # bogus hline as initial state.
        set vstate "" ; # Initial state of visual FSM
        set count 0
        variable html_frag
        
        foreach {mode text} $s {
            switch -exact -- $mode {
                {}    {append result [quote $text]}
                b - i {append result $html_frag($mode$text)}
                g {
                    if {$cgi == ""} {
                        append result "\[[quote $text]\]"
                        continue
                    }
                    if {$ip == ""} {
                        # no lookup, turn into a searchreference
                        append result \
                                $html_frag(a_) $cgi$text $html_frag(tc) \
                                [quote $text] $html_frag(_a)
                        continue
                    }
                    
                    set info [eval $ip [list $text]]
                    foreach {id name date} $info break
                    
                    if {$id == ""} {
                        # not found, don't turn into an URL
                        append result "\[[quote $text]\]"
                        continue
                    }
                    
                    regsub {^/} $id {} id
                    if {$date > 0} {
                        # exists, use ID
                        append result \
                                $html_frag(a_) $id $html_frag(tc) \
                                [quote $text] $html_frag(_a)
                        continue
                    }
                    
                    # missing, use ID -- editor link on the brackets.
                    append result \
                          $html_frag(a_) $id $html_frag(tc) \[ $html_frag(_a) \
                          [quote $text] \
                          $html_frag(a_) $id $html_frag(tc) \] $html_frag(_a) \
                        }
                u {
                    append result \
                            $html_frag(a_) $text $html_frag(tc) \
                            [quote $text] $html_frag(_a)
                }
                x {
                    if {[regexp {\.(gif|jpg|png)$} $text]} {
                        append result $html_frag(i_) $text $html_frag(tc)
                    } else {
                        append result \
                                \[ $html_frag(a_) $text $html_frag(tc) \
                                [incr count] $html_frag(_a) \]
                    }
                }
                T - Q - I - D - U - O - H {
                    append result $html_frag($state$mode)
                    set state $mode
                }
            }
        }
        # Close off the last section.
        append result $html_frag(${state}_)
        # Get rid of spurious newline at start of each quoted area.
        regsub -all "<pre>\n" $result "<pre>" result
        list $result {}
    }
    
    proc quote {q} {
        regsub -all {&} $q {\&amp;}  q
        regsub -all {"} $q {\&quot;} q ; # "
        regsub -all {<} $q {\&lt;}   q
        regsub -all {>} $q {\&gt;}   q
        regsub -all {&amp;(#\d+;)} $q {\&\1}   q
        return $q
    }
    
    # Define inter-section tagging, logical vertical space used between each 
	# logical section of text.
    #		| Current              (. <=> 1)
    #  Last	| T  Q  U  O  I  D  H
    # ----------+----------------------
    #  Text   T | See below
    #  Quote  Q |
    #  Bullet U |
    #  Enum   O |
    #  Term   I |
    #  T/def  D |
    #  HRULE  H |
    # ----------+----------------------
    
    variable  html_frag
    proc vs {last current text} {
        variable html_frag
        set      html_frag($last$current) $text
        return
    }
    
    vs T T   </p><p> ;vs T Q  </p><pre> ;vs T U   </p><ul><li> ;vs T O   </p><ol><li>
    vs Q T </pre><p> ;vs Q Q \n         ;vs Q U </pre><ul><li> ;vs Q O </pre><ol><li>
    vs U T  </ul><p> ;vs U Q </ul><pre> ;vs U U         \n<li> ;vs U O  </ul><ol><li>
    vs O T  </ol><p> ;vs O Q </ol><pre> ;vs O U  </ol><ul><li> ;vs O O         \n<li>
    vs I T  </dl><p> ;vs I Q </dl><pre> ;vs I U  </dl><ul><li> ;vs I O  </dl><ol><li>
    vs D T  </dl><p> ;vs D Q </dl><pre> ;vs D U  </dl><ul><li> ;vs D O  </dl><ol><li>
    vs H T       <p> ;vs H Q      <pre> ;vs H U       <ul><li> ;vs H O       <ol><li>
    
    vs T I   </p><dl><dt> ;vs T D   </p><dl><dd>  ;vs T H   "</p><hr size=1>"  ;vs T _   </p>
    vs Q I </pre><dl><dt> ;vs Q D </pre><dl><dd>  ;vs Q H "</pre><hr size=1>"  ;vs Q _ </pre>
    vs U I  </ul><dl><dt> ;vs U D  </ul><dl><dd>  ;vs U H  "</ul><hr size=1>"  ;vs U _  </ul>
    vs O I  </ol><dl><dt> ;vs O D  </ol><dl><dd>  ;vs O H  "</ol><hr size=1>"  ;vs O _  </ol>
    vs I I           <dt> ;vs I D           <dd>  ;vs I H  "</dl><hr size=1>"  ;vs I _  </dl>
    vs D I           <dt> ;vs D D           <dd>  ;vs D H  "</dl><hr size=1>"  ;vs D _  </dl>
    vs H I       <dl><dt> ;vs H D       <dl><dd>  ;vs H H       "<hr size=1>"  ;vs H _     {}
    rename vs {}
    
    array set html_frag {
	a_ {<a href="}  b0 </b>
	_a {</a>}       b1 <b>
        i_ {<img src="} i0 </i>
	tc {">}         i1 <i>
    } ; # "
    
    # =========================================================================
    # =========================================================================
    
    ### Backend renderer                                 :: Stream ==> Refs ###
    
    # =========================================================================
    # =========================================================================
    
    # Output specific conversion. Extracts all wiki internal page references
    # from the token stream and returns them as a list of page id's.
    
    proc StreamToRefs {s ip} {
        array set pages {}
        
        foreach {mode text} $s {
            if {![string equal $mode g]} {continue}
            
            set info [eval $ip [list $text]]
            foreach {id name date} $info break
            if {$id == ""} {continue}
            
            regexp {[0-9]+} $id id
            set pages($id) ""
        }
        
        array names pages
    }
    
    # Output specific conversion. Extracts all external references
    # from the token stream and returns them as a list of urls.
    
    proc StreamToUrls {s} {
        array set urls {}
        foreach {mode text} $s {
            if {$mode eq "u"} { set urls($text) imm }
            if {$mode eq "x"} { set urls($text) ref }
        }
        array get urls
    }
    
} ;# end of namespace

### Local Variables: ***
### mode:tcl ***
### tcl-indent-level:2 ***
### tcl-continued-indent-level:2 ***
### indent-tabs-mode:nil ***
### End: ***


