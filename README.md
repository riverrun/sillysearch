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

Add `-x py` to only search files with the py file extension.

To replace "wisible" with "risible", run:

    sillysearch wisible -r risible

Run `sillysearch --help` to see all the options.
