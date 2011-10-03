# zlib.tcl --
#
#	Low-level wrapper around libz, making it a Tcl package.
#
# Copyright (c) 2011 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# Example of exporting a C-level stubs API through critcl v3, linking
# against an external library and/or baking in the library using its sources.

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.4
package require critcl 3 ;# stubs management

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {A C-level wrapper of the zlib compression library}
critcl::description {
    This package wraps around zlib, making its
    C-level functions available as Tcl package,
    with stubs. No Tcl-binding is provided.
}

critcl::subject gzip zip zlib libz
critcl::subject compression {data compression}
critcl::subject decompression {data decompression}

# # ## ### ##### ######## ############# #####################
## Configuration

critcl::userconfig define mode {
    choose the zlib to build and link against.
} {
    system
    system-static
    local
}

# # ## ### ##### ######## ############# #####################
## Exported API

critcl::api scspec ZEXTERN

# # ## ### ##### ######## ############# #####################
##  Misc. interfaces

critcl::api function {const char *} zlibVersion {}
critcl::api function {const char *} zError      {int err}

critcl::api function uLong crc32 {
    uLong crc
    {const Bytef *} buf
    uInt len
}

critcl::api function uLong adler32 {
    uLong adler
    {const Bytef *} buf
    uInt len
}

# # ## ### ##### ######## ############# #####################
##  Deflate = Compression

critcl::api function int deflateInit_ {
    z_streamp      stream
    int            level
    {const char *} version
    int            stream_size
}

critcl::api function int deflateInit2_ {
    z_streamp stream
    int            level
    int            method
    int            windowBits
    int            memLevel
    int            strategy
    {const char *} version
    int            stream_size
}

critcl::api function int deflate {
    z_streamp stream
    int       flush
}

critcl::api function int deflateEnd {
    z_streamp stream
}

critcl::api function int deflateSetDictionary {
    z_streamp       stream
    {const Bytef *} dict
    uInt            dictLength
}

critcl::api function int deflateCopy {
    z_streamp dst
    z_streamp src
}

critcl::api function int deflateReset {
    z_streamp stream
}

critcl::api function int deflateParams {
    z_streamp stream
    int       level
    int       strategy
}

# # ## ### ##### ######## ############# #####################
##

critcl::api function int compress {
    {Bytef *}       dest
    {uLongf *}      destLen
    {const Bytef *} source
    uLong           sourceLen
}

critcl::api function int compress2 {
    {Bytef *}       dest
    {uLongf *}      destLen
    {const Bytef *} source
    uLong           sourceLen
    int             level
}

# # ## ### ##### ######## ############# #####################
##  Inflate = Decompression

critcl::api function int inflateInit_ {
    z_streamp      stream
    {const char *} version
    int            stream_size
}

critcl::api function int inflateInit2_ {
    z_streamp      stream
    int            windowBits
    {const char *} version
    int            stream_size
}

critcl::api function int inflate {
    z_streamp stream
    int       flush
}

critcl::api function int inflateEnd {
    z_streamp stream
}

critcl::api function int inflateSetDictionary {
    z_streamp       stream
    {const Bytef *} dict
    uInt            dictLength
}

critcl::api function int inflateSync  {z_streamp stream}
critcl::api function int inflateReset {z_streamp stream}

# # ## ### ##### ######## ############# #####################
##

critcl::api function int uncompress {
    {Bytef *}       dest
    {uLongf *}      destLen
    {const Bytef *} source
    uLong           sourceLen
}

# # ## ### ##### ######## ############# #####################
## gz'ip layer

critcl::api function gzFile gzopen {
    {const char *} path
    {const char *} mode
}

critcl::api function gzFile gzdopen {
    int            fd
    {const char *} mode
}

critcl::api function int gzsetparams {
    gzFile file
    int    level
    int    strategy
}

critcl::api function int gzread {
    gzFile   file
    voidp    buf
    unsigned len
}

critcl::api function int gzwrite {
    gzFile   file
    voidpc   buf
    unsigned len
}

critcl::api function int gzprintf {
    gzFile file
    {const char *} format
    ...
}

critcl::api function int gzputs {
    gzFile         file
    {const char *} s
}

critcl::api function {char *} gzgets {
    gzFile   file
    {char *} buf
    int      len
}

critcl::api function int gzputc {
    gzFile file
    int    c
}

critcl::api function int gzgetc {
    gzFile file
}

critcl::api function int gzflush {
    gzFile file
    int    flush
}

critcl::api function z_off_t gzseek {
    gzFile   file
    z_off_t offset
    int     whence
}

critcl::api function int     gzrewind {gzFile file}
critcl::api function z_off_t gztell   {gzFile file}
critcl::api function int     gzeof    {gzFile file}
critcl::api function int     gzclose  {gzFile file}

critcl::api function {const char *} gzerror {
    gzFile  file
    {int *} errnum
}

# # ## ### ##### ######## ############# #####################
## Implementation.

namespace eval ::zlib {}

# Export zlib version number. Together with the 'variant' (see below),
# we know what this package is an interface to.
critcl::cproc ::zlib::version {} vstring {
    return zlibVersion ();
}

critcl::cdata ::zlib::variant [critcl::userconfig query mode]
critcl::msg -nonewline " /[critcl::userconfig query mode]"

switch -exact -- [critcl::userconfig query mode] {
    local {
	# Build against the local z/lib/z sources.
	critcl::api header zlib/zconf.h
	critcl::api header zlib/zlib.h

	critcl::cheaders zlib/*.h
	critcl::csources zlib/adler32.c
	critcl::csources zlib/compress.c
	critcl::csources zlib/crc32.c
	critcl::csources zlib/deflate.c
	critcl::csources zlib/gzclose.c
	critcl::csources zlib/gzlib.c
	critcl::csources zlib/gzread.c
	critcl::csources zlib/gzwrite.c
	critcl::csources zlib/infback.c
	critcl::csources zlib/inffast.c
	critcl::csources zlib/inflate.c
	critcl::csources zlib/inftrees.c
	critcl::csources zlib/trees.c
	critcl::csources zlib/uncompr.c
	critcl::csources zlib/zutil.c
    }
    system {
	# Build against system z/lib/z

	critcl::api extheader zconf.h
	critcl::api extheader zlib.h

	critcl::clibraries -lz
    }
    system-static {
	# Build against system z/lib/z, statically

	critcl::api extheader zconf.h
	critcl::api extheader zlib.h

	set ok 0
	foreach p {
	    /lib
	    /usr/lib
	    /usr/local/lib
	} {
	    if {![file exists $p/libz.a]} continue
	    critcl::clibraries $p/libz.a
	    set ok 1
	    break
	}
	if {!$ok} {
	    critcl::error "Unable to find static libz.a"
	}
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide zlib 1 ; # for libz 1.2.5
