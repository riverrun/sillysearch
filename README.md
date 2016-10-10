# Sillysearch

A code repository search / replace tool written in Haskell.

Sillysearch is designed to search for, or replace, text in code repositories,
working through subdirectories recursively.

It uses regular expressions to search for, or replace, text.

## Installation

If you are using stack, just clone the repository and run `stack install`.

## Use

To search for the text "wisible" in the current directory, run:

    sillysearch wisible

Add `-d name_of_directory` to the above command to search a different directory.

To replace "wisible" with "risible", run:

    sillysearch wisible -r risible

To only search files with a certain file extension, use the `-x` option.

    sillysearch wisible -x py

    sillysearch wisible -x ex -x eex

Run `sillysearch -h` to see all the options.
