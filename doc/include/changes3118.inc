[section {Changes for version 3.1.18 (unreleased)}]
[list_begin enumerated]

[enum] Bugfix. Package [package critcl::cutil] bumped to version 0.1.1.
       Fixed missing inclusion of header [file string.h] in
       [file critcl_alloc.h], needed for [fun memcpy] in macro [fun STREP].

[enum] Merged pull request #83 from apnadkarni/vc-fixes.
       Removed deprecated -Gs for MSVC builds, and other Windows fixups.

[enum] Feature. Package [package critcl::iassoc] bumped to vresion 1.1.
       Refactored internals to generate an include header for use by .c files.
       This now matches what other generator packages do.
       The template file is inlined and removed.
       
[enum] Merged pull request #82 from gahr/home-symlink
       Modified tests to handle possibility of $HOME a symlink.

[enum] Merged pull request #81 from gahr/test-not-installed
       Modified test support to find uninstalled critcl packages when
       running tests. Handles all but critcl::md5.

[list_end]
