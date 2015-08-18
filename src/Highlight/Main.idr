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
   do (Just fn) <- getFilename
         | _ => usage
      putStrLn fn
      let idr = fn ++ ".idr"
      let idh = fn ++ ".idh"
      let tex = fn ++ ".tex"
      let html = fn ++ ".html"
      info <- readFile idh
      case parse expr info of
        Left err => do putStrLn err
        Right (SList xs) =>
          do let hls = mkHls $ catMaybes $ map getRegionMeta xs
             texFile <- openFile tex Write
             fwrite texFile (highlight LaTeX !(readFile idr) hls)
             closeFile texFile
             htmlFile <- openFile html Write
             putStrLn $ highlight HTML !(readFile idr) hls
             closeFile htmlFile
        Right meta =>
          printLn meta
      -- repl "> " (\input => case parse expr input of
      --                        Left err => "Error: " ++ err
      --                        Right expr => show expr)

