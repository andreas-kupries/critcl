# Compiled Runtime In Tcl

 *  This is version 2.1 of Critcl. It is API-compatible with Critcl
    version 2, with some extensions. It is a bug fix release with
    a major reworking of the internals hidden underneath.

# Building Critcl

 *  The toplevel directory contains a Tcl script **brew.tcl** for
    building and installing Critcl in various forms. Here we describe
    only building and installation of the necessary packages, and the
    creation of critcl starkits. For more information run

    ```% ./brew.tcl help```

 *  Building and installing the package, and its support:

    ```% ./brew.tcl install```

    builds and installs the critcl package, and all supporting packages
    in the **[info library]** directory of the **tclsh** found in PATH and
    used to run brew.tcl.

    Explicitly using a specific **tclsh**, like

    ```% /path/to/tclsh ./brew.tcl install```

    will install the packages in the **[info library]** directory of
    that shell.

    Explicitly using a specific installation directory, like

    ```% ./brew.tcl install /path/to/chosen/package/directory/```

    will install all the packages there.

 *  Building a starkit is don via

    ```% ./brew.tcl starkit```

    This places the resulting file **critcl.kit** in the current
    working directory.

    Explicitly using a specific installation path, like

    ```% ./brew.tcl starkit /path/to/chosen/critcl.kit```

    will put the generated starkit there.

# Changes

 *  Please read the 'Changes' sections in the reference manpages, or
    the files "doc/include/changes\*.inc" which are the shared source
    of said sections.

# Documentation

 *  The source of the reference manpages for the main packages can be
    found in the sub-directory "doc/", with various (shared) text blocks
    under "doc/include/". They written using 'doctools' markup (See
    Tcllib's doctools packages). The files match the pattern
    "doc/\*.man", and "doc/include/\*.inc".

 *  The files matching the pattern "doc/include/\*.dia" are diagram
    specifications using the 'dia' markup language (See Tklib's diagram
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
