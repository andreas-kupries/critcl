# Compiled Runtime In Tcl

 *  This is version 3 of Critcl, with several new features, listed
    below. This version has changes to the public API which make it
    incompatible with packages using Critcl version 2.x, or earlier.

# Building Critcl

 *  The toplevel directory contains a Tcl script **build.tcl** for
    building and installing Critcl in various forms. Here we describe
    only building and installation of the necessary packages, and the
    creation of critcl starkits. For more information run

    ```% ./build.tcl help```

 *  Building and installing the package, and its support:

    ```% ./build.tcl install```

    builds and installs the critcl package, and all supporting packages
    in the **[info library]** directory of the **tclsh** found in PATH and
    used to run build.tcl.

    Explicitly using a specific **tclsh**, like

    ```% /path/to/tclsh ./build.tcl install```

    will install the packages in the **[info library]** directory of
    that shell.

    Explicitly using a specific installation directory, like

    ```% ./build.tcl install /path/to/chosen/package/directory/```

    will install all the packages there.

 *  Building a starkit is don via

    ```% ./build.tcl starkit```

    This places the resulting file **critcl.kit** in the current
    working directory.

    Explicitly using a specific installation path, like

    ```% ./build.tcl starkit /path/to/chosen/critcl.kit```

    will put the generated starkit there.

# Using Critcl

 *  It is expected that the system you wish to use critcl on has a
    working C compiler. Installation and setup of such a compiler is
    platform and vendor specific, and instructions for doing so are
    very much outside of the scope of this document. Please read the
    documentation, how-tos, etc. for your platform or vendor.

# New Features

 *  Here we provide only a short list of the features. For more details
    see the 'Changes' sections in the reference manpages, or the files
    "doc/include/changes\*.inc" which are the shared source of said
    sections.

 *  Declaration, export and import of C-APIs through stubs tables.

 *  Generation of source packages from critcl-based code containing a
    TEA-based buildsystem wrapped around the raw critcl.

 *  Declaration, initializaton and use of user-specified configuration
    options. An important use is the declaration and use of custom
    build configurations, like 'link a 3rd party library dynamically,
    statically, build it from copy of its sources, etc.', etc.

# Documentation

 *  The source of the reference manpages for the main packages can be
    found in the sub-directory "doc/", with various (shared) text blocks
    under "doc/include/". They are written using *doctools* markup (See
    Tcllib's doctools packages). The files match the pattern
    "doc/\*.man", and "doc/include/\*.inc".

 *  The files matching the pattern "doc/include/\*.dia" are diagram
    specifications using the *dia* markup language (See Tklib's diagram
    packages). The files matching the pattern "doc/include/\*.png" are
    the diagrams saved as raster images in the PNG format.

    This was done with the **dia** application found in Tklib.

 *  The directory "embedded/" contains the reference manpages in nroff
    and HTML formats, derived from the doctools sources under "doc/".

    The script used to (re)generate these files is "tools/makedoc.sh".
    It requires the **dtplite** application found in Tcllib.

# History

 *  **2011-08-18** : Move code to public repository on GitHub

    The Subversion repository at *svn://svn.equi4.com/critcl* is now obsolete.  
    GitHub has the new official repository for Critcl.
