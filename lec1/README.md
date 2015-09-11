BAHUG101 - Lecture 1
====================

The first presentation for the BAHUG101 course. It covers

- What makes Haskell, Haskell
- Basic Syntax and Types
- List syntax
- Functions
- Basic error message reading

Included in the parent directory a nix shell file to make it
convenient to get into the correct environment to both compile and run
the included code.


Compiling
---------

To compile the presentation into a PDF, run

    make pdf

This will require custom fonts to be installed. If you do not have these
fonts, edit the Makefile to remove the fonts header added to the `tex`
target pandoc call.
