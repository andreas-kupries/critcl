[subsection {Building And Installing A Package}]

Here we assume that you know the basics of how to build a package.
If not, please read section [sectref {Building A Package}] first.

[para]

By default [cmd critcl] will place all newly-made packages in the
subdirectory [file lib] of the current working directory.

I.e. running

[example {
    critcl -pkg foo
}]

will create the directory [file lib/foo] which contains all the files
of the package.

[para]

When this behaviour is unwanted the option [option -libdir] is available,
allowing the explicit specification of the destination location to use.

[example {
    critcl -pkg -libdir /path/to/packages foo
}]

A common use might be to not only build the package in question, but
to also immediately install it directly in the path where the user's
[cmd tclsh] will be able to find it.

Assuming, for example, that the [cmd tclsh] in question is installed
at [file /path/to/bin/tclsh], with the packages searched for under
[file /path/to/lib] ([lb][cmd {info library}][rb]), the command

[example {
    critcl -pkg -libdir /path/to/lib foo
}]

will build the package and place it in the directory
[file /path/to/lib/foo].
