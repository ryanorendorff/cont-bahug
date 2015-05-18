Continuation Presentation for the Bay Area Haskell Users Group
==============================================================

Presentation on continuations for the Bay Area Haskell Users Group on 12
May, 2015.


Compiling
---------

To compile the presentation into a PDF, run

    make pdf

This will require custom fonts to be installed. If you do not have these
fonts, edit the Makefile to remove the fonts header added to the `tex`
target pandoc call.


Running
-------

The code inside the presentation can be run using

    make run

This will require `runhaskell` to be in your path.
