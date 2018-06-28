[section {Changes for version 3.1.18 (unreleased)}]
[list_begin enumerated]

[enum] Bugfix. Package [package critcl::cutil] bumped to version 0.1.1.
       Fixed missing inclusion of header [file string.h] in
       [file critcl_alloc.h], needed for [fun memcpy] in macro [fun STREP].

[enum] Merged pull request #83 from apnadkarni/vc-fixes.
       Removed deprecated -Gs for MSVC builds, and other Windows fixups.

[enum] Feature. Package [package critcl::iassoc] bumped to version 1.1.
       Refactored internals to generate an include header for use by .c files.
       This now matches what other generator packages do.
       The template file is inlined and removed.
       
[enum] Merged pull request #82 from gahr/home-symlink
       Modified tests to handle possibility of $HOME a symlink.

[enum] Merged pull request #81 from gahr/test-not-installed
       Modified test support to find uninstalled critcl packages when
       running tests. Handles all but critcl::md5.

[enum] Merged pull request #85 from snoe925/issue-84
       to fix Issue #84 breaking installation on OSX.

[enum] Merged pull request #87 from apnadkarni/tea-fixes to fix Issue
       #86, broken -tea option, generating an incomplete package.

[enum] Feature. New package [package critcl::callback] providing
       C-level functions and data structures to manage callbacks from
       C to Tcl.

[enum] Feature. Package [package critcl::literals] bumped to version
       1.3.  Added mode [const multi] enabling the conversion of
       multiple literals into a list of their strings.

[list_end]
