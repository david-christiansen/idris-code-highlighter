A source code highlighter for Idris
===================================

This is a quick hack that I threw together - contributions to make it less user-hostile are welcome! It serves my needs for now, so I'm putting it out there.

When Idris is run with the `--highlight` flag, it generates a file with the `.idh` extension that contains the same kinds of semantic highlighting information that it sends to editor clients such as Emacs or Atom. This little script parses that file and then generates HTML and LaTeX sources corresponding to the source code.

To use it, pass it the base name of the `.idr` and `.idh` files (that is, without the extensions). It will then build the highlighted files. If you give it an incorrectly formatted argument, it will crash - please be warned!  Obviously, this should be better.

To build it, use `idris --build idris-code-highlighter.ipkg`. It needs [Lightyear](https://github.com/ziman/lightyear) to be installed already.  You'll need a recent Git version of Idris for this to work well.

Note that Idris highlighting information contains filenames relative to the source directory (specifically, the compiler's working directory while building). This means that you must run `idrishl` with the same working directory that Idris had when it was building, or else the filename information won't match and the highlighting will be rejected.

For a demo, try the `hl-me.sh` script. It generates highlighted versions of the program's own source.

Development
-----------
To get started, it might be worth taking a look at what I think are good first projects: there is a "Low-Hanginging Fruit" tag in the issue tracker. If you have any questions, please don't hesitate to get in touch.
