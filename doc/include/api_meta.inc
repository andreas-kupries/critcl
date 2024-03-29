
CriTcl versions 3 and later can create TEApot meta-data to be placed into
[file teapot.txt] in a format suitable for use by the
[uri {http://docs.activestate.com/activetcl/8.5/tpm/toc.html} {TEApot tools}].

[para]
In version 2, some meta data support was already present through
[cmd ::critcl::license], but this was only used to generate [file license.txt].


[list_begin definitions]
[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::license] [arg author] [opt [arg text]...]]

Ignored in "compile & run" mode.

[para]
In "generate package" mode provides information about the author of the
package and the license for the package.

[para]
[arg text] arguments are concatenated to form the text of the license, which is
written to [file license.terms] in the same directory as [file pkgIndex.tcl].
If no [arg text] is provided the license is read from [file license.terms]
in the same directory as the [vset critcl_script].

[para] This information takes precedence over any information specified through
the generic API [cmd ::critcl::meta].  It is additionally placed
into the meta data file [file teapot.txt] under the keys [term as::author] and
[term license].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::summary] [arg text]]

Ignored in "compile & run" mode.

[para]
In "generate package" mode places a short, preferably one-line description of
the package into the meta data file [file teapot.txt] under the key
[term summary].  This information takes precedence over information specified
through the generic API [cmd ::critcl::meta].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::description] [arg text]]

Ignored in "compile & run" mode.

[para]
In "generate package" mode places a longer description of the package into the
meta data file [file teapot.txt], under the key [term description].  The data
specified by this command takes precedence over any information specified
through the generic API [cmd ::critcl::meta].

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::subject] [opt [arg key]...]]

Ignored in "compile & run" mode.

[para]
In "generate package" mode places each [arg key] into the meta data file
[file teapot.txt], under the key [term subject].  This information takes
precedence over any information specified through the generic API
[cmd ::critcl::meta].

[para] Calls to this command are cumulative.


[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::meta] [arg key] [opt [arg word]...]]

Provides arbitrary meta data outside of the following reserved keys:

[term as::author],
[term as::build::date],
[term description],
[term license],
[term name],
[term platform],
[term require]
[term subject],
[term summary], and
[term version],

Its behaviour is like [cmd ::critcl::subject] in that it treats all
keys as list of words, with each call providing one or more words for
the key, and multiple calls extending the data for an existing key, if
not reserved.

[para] While it is possible to declare information for one of the
reserved keys with this command such data is ignored when the final
meta data is assembled and written.

[para] Use the commands
[cmd ::critcl::license],
[cmd ::critcl::summary],
[cmd ::critcl::description]
[cmd ::critcl::subject],
[cmd {package require}], and
[cmd {package provide}]
to declare data for the reserved keys.

[para] The information for the reserved keys

[term as::build::date] and
[term platform]

is automatically generated by [package critcl] itself.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::meta?] [arg key]]

Returns the value in the metadata associated with [arg key].

[para] Used primarily to retrieve the name of the package
from within utility packages having to adapt C code templates to their
environment. For example, [package critcl::class] uses does this.

[comment ---------------------------------------------------------------------]
[call [cmd ::critcl::buildrequirement] [arg script]]

Provides control over the capturing of dependencies declared via
[cmd {package require}]. [arg script] is evaluated and any dependencies
declared within are ignored, i.e. not recorded in the meta data.

[list_end]
