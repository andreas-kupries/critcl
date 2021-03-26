[section {Changes for version 3.1.18.2}]

[strong {Not Released Yet}]

[list_begin enumerated]

[enum] [strong {Performance Fix}] for [term {compile & run}] mode.
       Issue #112.

[para] Moved the command activating more precise code location
       tracking out of package [package critcl] into package
       [package critcl::app].

[para] The ahead of time compilation of the [term package] mode done
       by the application (package) can bear the performance penalty
       invoked by this [strong global] setting.

[para] Arbitrary libraries and applications using critcl dynamically,
       i.e. in [term {compile & run}] likely cannot, and should not.

[list_end]
