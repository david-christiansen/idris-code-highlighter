module Highlight.Main

import Highlight.Parser
import Highlight.Regions
import Highlight.Formats
import Lightyear.Strings

usage : IO ()
usage = putStrLn "Usage:" *>
        putStrLn "highlight-idris BASENAME" *>
        putStrLn "where BASENAME.idr and BASENAME.idh both exist. The output will be BASENAME.tex and BASENAME.html."


getFilename : IO (Maybe String)
getFilename = case !getArgs of
                [prog, filename] => return (Just filename)
                _ => return Nothing

main : IO ()
main =
   do (Just fn') <- getFilename
         | _ => usage
      let fn = if isSuffixOf ".idh" fn'
                 then pack (reverse (drop 4 (reverse (unpack fn'))))
                 else fn'
      let idr = fn ++ ".idr"
      let idh = fn ++ ".idh"
      let tex = fn ++ ".tex"
      let html = fn ++ ".html"
      Right info <- readFile idh
        | Left err => printLn err
      case parse expr info of
        Left err => do putStrLn err
        Right (SList xs) =>
          do let hls = mkHls $ catMaybes $ map getRegionMeta xs
             let realHls = sort (filter (\r => fileName r == idr) hls)
             Right idrContents <- readFile idr
               | Left err => printLn err
             Right texFile <- openFile tex WriteTruncate
               | Left err => printLn err
             fPutStr texFile (highlight LaTeX idrContents realHls)
             closeFile texFile
             Right htmlFile <- openFile html WriteTruncate
               | Left err => printLn err
             fPutStr htmlFile (highlight HTML idrContents realHls)
             closeFile htmlFile
        Right meta =>
          printLn meta
