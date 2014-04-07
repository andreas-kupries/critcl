
[subsection {C compiler}]

To actually build packages based on critcl we need a working C
compiler.

[para] How to install such and all the associated header files,
libraries, etc. is heavily platform- and system-dependent, and thus
outside of the scope of this document.

Note that I am willing to extend this section with links of interest
to tutorials, howtos and references for the various platforms.

[para] The important pieces of information are this:

[list_begin enumerated]

[enum]	The path to the C compiler binary must be found in the
	environment variable [var PATH], for critcl to find it.

[enum]	On Windows(tm) the environment variable [var LIB] must be
	present and contain the paths of the directories holding
	Microsoft's libraries. The standard critcl configuration
	for this platform searches these paths to fine-tune its
	settings based on available libraries and compiler version.

[list_end]

[para] Links of interest:

[list_begin definitions]
[def [url http://www.tldp.org/HOWTO/HOWTO-INDEX/programming.html]]
[list_end]
