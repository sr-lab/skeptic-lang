module SkepticLang.IO


%access public export


||| Loads lines from a text file.
||| @path   the path to the file
load_lines : (path : String) -> IO (Maybe (List String))
load_lines path = do
  txt <- readFile path
  case txt of
    Right txt' => pure (Just (lines txt'))
    _ => pure Nothing
