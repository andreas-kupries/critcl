[subsection {Introspection of Targets And Configurations}]

To see a list containing the names of all the available
configurations, run

[example {
    critcl -targets
}]

The configuration settings for either the default or user-chosen
target can be inspected on [const stdout] with

[example {
    critcl -show
}]

and

[example {
    critcl -show -target TARGET
}]

The raw contents of the configuration file used by [cmd critcl]
are dumped to [const stdout] with

[example {
    critcl -showall
}]

All of the above can of course be combined with custom configuration
files. [comment {%% Write section about this %%}]
