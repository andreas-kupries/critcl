[section {Changes for version 3.1.10}]
[list_begin enumerated]

[enum]	Fixed code version numbering forgotten with 3.1.9.

[enum]	Fixed issue #35. In package mode (-pkg) the object cache
	directory is unique to the process, thus we do not need
	content-hashing to generate unique file names. A simple
	counter is sufficient and much faster.
[para]
	Note that mode "compile & run" is not as blessed and still
	uses content-hasing with md5 to ensure unique file names
	in its per-user object cache.

[list_end]
