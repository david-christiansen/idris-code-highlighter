module Highlight.Main

import Effects
import Effect.File
import Highlight.Parser
import Highlight.Regions
import Highlight.Formats
import Lightyear.Strings

---------------------
-- Usage Instructions
---------------------

||| Print usage instructions
usage : IO ()
usage = putStrLn "Usage:" *>
        putStrLn "highlight-idris BASENAME" *>
        putStr   "where BASENAME.idr and BASENAME.idh both exist. " *>
        putStrLn "The output will be BASENAME.tex and BASENAME.html."

---------------
-- Highlighting
---------------

||| Extract a sorted list of highlights from a parsed Idris highlight file
||| @ file the filename to which highlights apply
||| @ hls the highlighting information
sortedHighlights : (file : String) ->
                   (hls  : List SExpr) ->
                   List (Region HighlightType)
sortedHighlights file hls =
    let metadata = map getRegionMeta hls
        successfulMetadata = catMaybes metadata
        hls = mkHls successfulMetadata
        forThisFile = filter ((== file) . fileName) hls
    in sort forThisFile

||| Highlight `src` in `format` using `highlights` and write to `outputFile`.
||| @ outputFile The file to which to write the formatted highlights.
||| @ format     The output `Format`.
||| @ src        The contents of the Idris source file.
||| @ highlights A list of highlights for `src`.
doHighlight : (outputFile : String) ->
              (format     : Format) ->
              (src        : String) ->
              (highlights : List (Region HighlightType)) ->
              Eff (IO ()) [FILE ()]
doHighlight outputFile format src highlights =
    do Success <- writeFile outputFile (highlight format src highlights)
         | FError err => pure (printLn err)
       pure (pure ())

||| Generate `basename`.tex and `basename`.html via `doHighlight`.
doHighlights : (basename : String) -> Eff (IO ()) [FILE ()]
doHighlights bn =
    do Result info <- readFile (bn ++ ".idh")
         | FError err => pure (printLn err)
       case parse expr info of
         Left err => pure (putStrLn err)
         Right (SList xs) =>
           do let idr = bn ++ ".idr"
              Result src <- readFile idr
                | FError err => pure (printLn err)
              let hls = sortedHighlights idr xs
              doHighlight (bn ++ ".tex")  LaTeX src hls
              doHighlight (bn ++ ".html") HTML  src hls

-------------------------
-- Figuring out filenames
-------------------------

||| Get the file name from the command line arguments.
getFilename : IO (Maybe String)
getFilename = case !getArgs of
                [prog, filename] => return (Just filename)
                _                => return Nothing

||| Strip the ".idh" extension from a file's name.
basename : (file : String) -> String
basename file =
    if isSuffixOf ".idh" file
      then pack (reverse (drop 4 (reverse (unpack file))))
      else file

||| Get the base filename from the command line arguments.
getBasename : IO (Maybe String)
getBasename = map (map basename) getFilename

main : IO ()
main =
    do Just basename <- getBasename | _ => usage
       run (doHighlights basename)
       putStrLn ("Processed " ++ basename)

