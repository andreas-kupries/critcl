#!/bin/sh
# tools
# % generate the embedded documentation.

# 1. html from the doctools
# 2. nroff from the doctools

(   cd doc
    echo ___ MAN _________
    rm -rf     ../embedded/man
    mkdir      ../embedded/man
    dtplite -ext n -o ../embedded/man nroff .

    echo ___ WWW _________
    rm -rf     ../embedded/www
    mkdir      ../embedded/www
    dtplite -o ../embedded/www html  .
)

echo ___ MAN /show _________
less embedded/man/files/*.n

exit
