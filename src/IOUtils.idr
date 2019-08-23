module SkepticLang.IOUtils


%access public export


||| Loads lines from a text file.
||| @path   the path to the file
load_lines : (path : String) -> IO (Maybe (List String))
load_lines path = do
  txt <- readFile path
  case txt of
    Right txt' => pure (Just (lines txt'))
    _ => pure Nothing


unsplit : (delim : String) -> (strs : List String) -> String
unsplit _ [] = ""
unsplit delim (str :: strs') = str ++ delim ++ unsplit delim strs'


dirName : (path : String) -> String
dirName path =
  case split (== '/') path of -- Split path.
    (x :: xs) => unsplit "/" (init (x :: xs))
    _ => ""


isSlashTerminated : (path : String) -> Bool
isSlashTerminated path =
  case last' (unpack path) of
    Just '/' => True
    _ => False

joinPaths : (x : String) -> (y : String) -> String
joinPaths x y = x ++ (if isSlashTerminated x then "" else "/") ++ y


isAbsPath : (path : String) -> Bool
isAbsPath path =
  case unpack path of
    ('/' :: _) => True
    _ => False

toAbsPath : (base : String) -> (given : String) -> String
toAbsPath base given =
  if isAbsPath given then
    given
  else
    joinPaths base given
