module Highlight.Formats

import Highlight.Regions
import Control.Monad.State

||| A description of how to annotate a source file to output to a particular format.
public
record Format where
  constructor MkFormat
  openTag   : HighlightType -> String
  closeTag  : HighlightType -> String
  escape    : Char -> String
  preamble  : String
  postamble : String

private total
openTagL : HighlightType -> String
openTagL (Name Function _ _)        = "\\IdrisFunction{"
openTagL (Name Constructor _ _)     = "\\IdrisData{"
openTagL (Name TypeConstructor _ _) = "\\IdrisType{"
openTagL (Bound _)                  = "\\IdrisBound{"
openTagL Keyword                    = "\\IdrisKeyword{"

private total
closeTagL : HighlightType -> String
closeTagL _ = "}"

private total
escapeL : Char -> String
escapeL '{'  = "\\{"
escapeL '}'  = "\\}"
escapeL '\\' = "\\textbackslash{}"
escapeL c    = singleton c

private total
preambleL : String
preambleL =
  """\documentclass{article}
\usepackage{fancyvrb}
\usepackage[usenames]{xcolor}
\newcommand{\IdrisData}[1]{\textcolor{red}{#1}}
\newcommand{\IdrisType}[1]{\textcolor{blue}{#1}}
\newcommand{\IdrisBound}[1]{\textcolor{magenta}{#1}}
\newcommand{\IdrisFunction}[1]{\textcolor{green}{#1}}
\newcommand{\IdrisKeyword}[1]{{\underline{#1}}}
\newcommand{\IdrisImplicit}[1]{{\itshape \IdrisBound{#1}}}


\begin{document}
% START CODE
\begin{Verbatim}[commandchars=\\\{\}]
"""

private total
postambleL : String
postambleL =
  """\end{Verbatim}
% END CODE
\end{document}
"""

public total
LaTeX : Format
LaTeX = MkFormat openTagL closeTagL escapeL preambleL postambleL

private total
preambleH : String
preambleH =
  """<!doctype html><html><head><style>.idris-data { color: red; }
.idris-type { color: blue; }
.idris-function {color: green; }
.idris-keyword { font-weight: bold; }
.idris-bound { color: purple; }
.idris-implicit { font-style: italic; }
.idris-underlined { text-decoration: underline; }</style></head><body><!-- START CODE --><pre>"""

private total
postambleH : String
postambleH = "</pre><!-- END CODE --></body></html>"

private total
openTagH : HighlightType -> String
openTagH (Name Function d t)        = "<span class=\"idris-function\" title=\"" ++ d ++ "\n" ++ t ++ "\">"
openTagH (Name Constructor d t)     = "<span class=\"idris-data\" title=\"" ++ d ++ "\n" ++ t ++ "\">"
openTagH (Name TypeConstructor d t) = "<span class=\"idris-type\" title=\"" ++ d ++ "\n" ++ t ++ "\">"
openTagH (Bound i)                  = "<span class=\"idris-bound" ++
                                      (if i then " idris-implicit" else "") ++
                                      "\">"
openTagH Keyword                    = "<span class=\"idris-keyword\">"

private total
closeTagH : HighlightType -> String
closeTagH hl = "</span><!-- closing " ++ show hl ++ "-->"

private total
escapeH : Char -> String
escapeH '&' = "&amp;"
escapeH '<' = "&lt;"
escapeH '>' = "&gt;"
escapeH c = singleton c

public total
HTML : Format
HTML = MkFormat openTagH closeTagH escapeH preambleH postambleH

------------------------------------
-- Applying formats to file contents
------------------------------------
private
record HLState where
  constructor MkHlState
  lineNo : Integer
  colNo : Integer
  highlights : List (Region HighlightType)
  openHighlights : List (Region HighlightType)
  output : List String

private
incCol : HLState -> HLState
incCol st = record {colNo = 1 + colNo st} st

private
incLine : HLState -> HLState
incLine st = record {lineNo = 1 + lineNo st} st

private
doOutput : String -> State HLState ()
doOutput str = do st <- get
                  put (record {output = str :: output st} st)

private
openAll : (line, col : Integer) -> Format -> State HLState ()
openAll line col fmt = 
                   do hls <- highlights <$> get
                      let (toOpen, notYet) = List.span (\hl => startLine hl < line ||
                                                               (startLine hl == line &&
                                                                startColumn hl <= col))
                                                        hls
                      traverse_ (doOutput . openTag fmt . metadata) toOpen
                      st <- get
                      put (record { highlights = notYet
                                  , openHighlights = reverse toOpen ++ openHighlights st
                                  } st)

closeAll : (line, col : Integer) -> Format -> State HLState ()
closeAll line col fmt = 
                    do openHls <- openHighlights <$> get
                       let (toClose, notYet) = span (\hl => endLine hl < line ||
                                                          (endLine hl == line &&
                                                           endColumn hl <= col))
                                                    openHls
                       traverse_ (doOutput . closeTag fmt . metadata) toClose
                       st <- get
                       put (record {openHighlights = notYet} st)


||| Apply highlighting to a source file for some output format
public
highlight : Format -> (fileContents : String) -> List (Region HighlightType) -> String
highlight fmt file hls = preamble fmt ++
                         concat (evalState (highlight' (split isNL file) []) (MkHlState 0 0 (sort hls) [] [])) ++
                         postamble fmt
  where highlight' : List String -> List Char -> State HLState (List String)
        highlight' [] [] = do traverse_ (doOutput . closeTag fmt . metadata) (openHighlights !get)
                              return (reverse (output !get))
        highlight' lines (c::cs) = do modify incCol
                                      line <- lineNo <$> get
                                      col <- colNo <$> get
                                      openAll line col fmt
                                      closeAll line col fmt
                                      doOutput $ escape fmt c
                                      highlight' lines cs
        highlight' (l::lines) [] = do closeAll (lineNo !get) (colNo !get) fmt
                                      modify incLine
                                      modify (record {colNo = 0})
                                      doOutput "\n"
                                      highlight' lines (unpack l)

