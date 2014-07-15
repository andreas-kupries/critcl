#!/bin/sh

# Regenerate the figure holding the graph of dependencies...
dot -Tpng -o dependencies.png dependencies.dot

# ...and show it
display dependencies.png
exit
