module SkepticLang.IOUtils


%access public export


||| Loads lines from a text file.
|||
||| @path   the path to the file
loadLines : (path : String) -> IO (Maybe (List String))
loadLines path = do
  txt <- readFile path
  case txt of
    Right txt' => pure (Just (lines txt'))
    _ => pure Nothing


||| Joins a list of strings with the specified delimiter.
|||
||| @delim  the delimiter
||| @strs   the list of strings
unsplit : (delim : String) -> (strs : List String) -> String
unsplit _ [] = ""
unsplit delim (str :: strs') = str ++ delim ++ unsplit delim strs'


||| Gets the directory portion of a given file path.
|||
||| @path the file path
dirName : (path : String) -> String
dirName path =
  case split (== '/') path of -- Split path.
    (x :: xs) => unsplit "/" (init (x :: xs))
    _ => ""


||| Checks if a path is terminated with a slash.
|||
||| @path the path to check
isSlashTerminated : (path : String) -> Bool
isSlashTerminated path = last' (unpack path) == Just '/'


||| Joins two paths together.
|||
||| @x the base path
||| @y the path to append
joinPaths : (x : String) -> (y : String) -> String
joinPaths x y = x ++ (if isSlashTerminated x then "" else "/") ++ y


||| Returns true if the given path is absolute.
|||
||| @path the path to check
isAbsPath : (path : String) -> Bool
isAbsPath path = head' (unpack path) == Just '/'


||| Converts a relative path to an absolute path.
|||
||| @base the base directory
||| @rel  the relative path to convert
toAbsPath : (base : String) -> (rel : String) -> String
toAbsPath base rel =
  if isAbsPath rel then
    rel
  else
    joinPaths base rel


||| Resolves an absolute path from a relative path.
|||
||| @rel  the relative path to convert
resolveAbsPath : (rel : String) -> IO String
resolveAbsPath rel = do
  base <- currentDir
  pure (toAbsPath base rel)
