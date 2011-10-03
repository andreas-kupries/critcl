[subsection {Custom Header Path}]

Sometimes the use of [cmd critcl::headers] might not be enough for a
package to find its headers. Maybe they are outside of the paths
checked by the setup code.

To help the application recognizes the option [option -I] which allows
the user to supply a single additional include path to use during the
build phase of the package.

[para] Simply use

[example {
    critcl -I /path/to/header ...
}]

and the specified header will be handed to the package to be built.
