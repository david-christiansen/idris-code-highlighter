module Highlight.Main

import Effects
import Effect.File
import Highlight.Parser
import Highlight.Regions
import Highlight.Formats
import Lightyear.Strings

usage : IO ()
usage = putStrLn "Usage:"
     *> putStrLn "highlight-idris BASENAME"
     *> putStr   "where BASENAME.idr and BASENAME.idh both exist. "
     *> putStrLn "The output will be BASENAME.tex and BASENAME.html."

getFilename : IO (Maybe String)
getFilename = case !getArgs of
                [prog, filename] => return (Just filename)
                _                => return Nothing

doHighlights : (basename : String) -> Eff (Maybe String) [FILE ()]
doHighlights bn = do
  Result info <- readFile (bn ++ ".idh") | FError err => pure (Just (show err))
  case parse expr info of
    Left err => pure (Just err)
    Right (SList xs) => do
      let hls     = mkHls $ catMaybes $ map getRegionMeta xs
      let idr     = bn ++ ".idr"
      let realHls = sort (filter (\r => fileName r == idr) hls)
      Result src <- readFile idr
        | FError err => pure (Just (show err))
      Success <- writeFile (bn ++ ".tex") (highlight LaTeX src hls)
        | FError err => pure (Just (show err))
      Success <- writeFile (bn ++ ".html") (highlight HTML src hls)
        | FError err => pure (Just (show err))
      pure Nothing

main : IO ()
main = do
  Just fn' <- getFilename | _ => usage
  let fn = if isSuffixOf ".idh" fn'
             then pack (reverse (drop 4 (reverse (unpack fn'))))
             else fn'
  Nothing <- run $ doHighlights fn | Just err => putStrLn err
  putStrLn $ "Processed " ++ fn
