[section {Changes for version 3.1.18.2}]

[strong {Not Released Yet}]

[list_begin enumerated]

[enum] [strong {Performance Fix}] for [term {compile & run}] mode.
       Issue #112.

[para] Moved the command activating more precise code location
       tracking out of package [package critcl] into package
       [package critcl::app].

[para] Because generating packages ahead of time can bear the
       performance penalty invoked by this [strong global] setting.

[para] Arbitrary libraries and applications using critcl dynamically
       ([term {compile & run}]) on the other hand likely cannot, and
       should not.

[enum] Extended [cmd cproc] argument type processing. Now able to
       auto-create restricted scalar types. I.e. types derived from
       [term int], etc. and limited in the range of allowed values.

[para] Further able to auto-create restricted list types, i.e. types
       derived from [term list] and either limited in length, or in
       the type of the elements, or both.

[list_end]
