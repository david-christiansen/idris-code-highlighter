A source code highlighter for Idris
===================================

This is a quick hack that I threw together - contributions to make it less user-hostile are welcome! It serves my needs for now, so I'm putting it out there.

When Idris is run with the `--highlight` flag, it generates a file with the `.idh` extension that contains the same kinds of semantic highlighting information that it sends to editor clients such as Emacs or Atom. This little script parses that file and then generates HTML and LaTeX sources corresponding to the source code.

To use it, pass it the base name of the `.idr` and `.idh` files (that is, without the extensions). It will then build the highlighted files. If you give it an incorrectly formatted argument, it will crash - please be warned!  Obviously, this should be better.

To build it, use `idris --build idris-code-highlighter.ipkg`. It needs [Lightyear](https://github.com/ziman/lightyear) to be installed already.  You'll need a recent Git version of Idris for this to work well.

For a demo, try the `hl-me.sh` script. It generates highlighted versions of the program's own source.
