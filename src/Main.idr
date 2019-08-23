module SkepticLang.Main


import Language.JSON


import Lists
import Io


||| Core type representing a power-law equation.
public export
record Zipf where
  ||| Creates a power-law equation.
  |||
  ||| @amp    the scaling factor of the equation
  ||| @alpha  the exponent of the equation
  constructor MkZipf
  amp : Double
  alpha : Double


record Environment where
  constructor MkEnvironment
  equations : List (String, Zipf)
  groups : List (String, List (String, Zipf))


comp : (f : Zipf) -> (x : Double) -> Double
comp f x = (amp f) * (pow x (alpha f))


||| Computes the average slope of a function between two points.
||| ...
slope : (f : Zipf) -> (a : Double) -> (b : Double) -> Double
slope f a b = (abs ((comp f a) - (comp f b))) / (abs (a - b))


||| Adds a Zipf equation to the environment.
|||
||| @env  the environment to add the equation to
||| @name the name to add the equation under
||| @eq   the equation to add
addZipf : (env : Environment) -> (name : String) -> (eq : Zipf) -> Environment
addZipf env name eq = MkEnvironment ((name, eq) :: (equations env)) (groups env)


||| Gets a named Zipf equation from an environment.
|||
||| @env  the environment to get the equation from
||| @name the name of the equation to get
getZipf : (env : Environment) -> (name : String) -> Maybe Zipf
getZipf env name = lookup (equations env) name


addGroup : (env : Environment) -> (name : String) -> Environment
addGroup env name = MkEnvironment (equations env) ((name, []) :: (groups env))


getGroup : (env : Environment) -> (name : String) -> Maybe (List (String, Zipf))
getGroup env name = lookup (groups env) name


rmGroup : (env : Environment) -> (name : String) -> Environment
rmGroup env name = MkEnvironment (equations env) (filter (\(a, b) => a /= name) (groups env))



addToGroup : (env : Environment) -> (groupId : String) -> (eq : Zipf) -> (eqId : String) -> Environment
addToGroup env groupId eq eqId =
  case lookup (groups env) groupId of
    Just group =>
      let newGroup = ((eqId, eq) :: group) in
      MkEnvironment (equations env) ((groupId, newGroup) :: (rmId (groups env) groupId))
    Nothing => env



||| Gets the value of an attribute of a JSON object.
|||
||| @json the JSON object
||| @key  the name of the attribute
getObjAttr : (json : JSON) -> (key : String) -> Maybe JSON
getObjAttr (JObject xs) key = getObjAttr' xs key where
  getObjAttr' : (json : List (String, JSON)) -> (key : String) -> Maybe JSON
  getObjAttr' ((a, b) :: xs) key = if a == key then Just b else getObjAttr' xs key
  getObjAttr' [] _ = Nothing
getObjAttr _ _  = Nothing


||| Loads a Zipf equation from a file.
|||
||| @path the file from which to load the equation
loadEq : (path : String) -> IO (Maybe Zipf)
loadEq path = do
  txt <- readFile path
  case txt of
    Right txt' => case JSON.parse txt' of
      Just json => case (getObjAttr json "amp", getObjAttr json "alpha") of
        (Just (JNumber amp), Just (JNumber alpha)) => pure (Just (MkZipf amp alpha))
        _ => pure Nothing
      Nothing => pure Nothing
    _ => pure Nothing


||| Returns a function defining a relation between numbers from a name.
|||
||| @name the name to use for the lookup
getRelation : (name : String) -> Maybe (Double -> Double -> Bool)
getRelation name = case name of
  "shallower" => Just (>)
  "steeper" => Just (<)
  _ => Nothing


||| Evaluates a tokenized line of Skeptic assertion code.
|||
||| @env    the environment to evaluate in
||| @tokens the tokens to evaluate
skepticEvalLineTokens : (env : Environment) -> (tokens : List String) -> IO Environment
skepticEvalLineTokens env tokens =
  case tokens of
  ["zipf", amp, alpha, "as", name] =>
    pure (addZipf env name (MkZipf (cast amp) (cast alpha))) -- Declare Zipf equation directly.
  ["load", file, "as", name] => do
    eq <- loadEq file
    case eq of
      Just eq' => pure (addZipf env name eq')
      Nothing => do
        putStrLn "Could not load equation."
        pure env
  ["group", name] => pure (addGroup env name)
  ["add", eq, "to", gr, "as", name] =>
    case (getZipf env eq) of
      Just z => pure (addToGroup env gr z name)
  ["assert", x, r, y, "between", a, "and", b] => do
    case (getZipf env x, getZipf env y, getRelation r) of -- Look up equations.
      (Just x', Just y', Just r') =>
        if r' (slope x' (cast a) (cast b)) (slope y' (cast a) (cast b)) then
          putStrLn "Could not assert this."
        else
          putStrLn "Successfully asserted this."
      _ => putStrLn "Equation error."
    pure env
  ["assert", bx, x, r, by, y, "between", a, "and", b] => do
    case (getGroup env x, getGroup env y, getRelation r) of -- Look up equations.
      (Just x', Just y', Just r') =>
        case (lookup x' bx, lookup y' by) of
          (Just xx', Just yy') =>
            if r' (slope xx' (cast a) (cast b)) (slope yy' (cast a) (cast b)) then
              putStrLn "Could not assert this."
            else
              putStrLn "Successfully asserted this."
          _ => putStrLn "ERR"
      _ => putStrLn "Equation error."
    pure env
  _ => do
    putStrLn "Parse error."
    pure env


||| Remvoes empty strings from a list.
|||
||| @tokens the list to filter
stripEmptyTokens : (tokens : List String) -> List String
stripEmptyTokens tokens = filter (\x => length x > 0) tokens


||| Evaluates a line of Skeptic assertion code.
|||
||| @env  the environment to evaluate in
||| @line the line to evaluate
skepticEvalLine : (env : Environment) -> (line : String) -> IO Environment
skepticEvalLine env line =
  case unpack line of
    ('#' :: _) => pure env -- Ignore comments.
    _ =>
      case stripEmptyTokens (split (== ' ') line) of -- Split along spaces to create token list.
        [] => pure env -- Ignore blank lines.
        tokens => skepticEvalLineTokens env tokens


skepticEvalLines : (env : Environment) -> (lines : List String) -> IO ()
skepticEvalLines env (line :: lines') = do
  env' <- skepticEvalLine env line
  skepticEvalLines env' lines'
skepticEvalLines _ [] = pure ()


||| Evaluates a file of Skeptic assertion code.
|||
||| @path the lines to evaluate
skepticEvalFile : (path : String) -> IO ()
skepticEvalFile path = do
  lines <- load_lines path
  case lines of
    Just lines' => skepticEvalLines (MkEnvironment [] []) lines'
    Nothing => putStrLn "File read error."
