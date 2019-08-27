module SkepticLang.Main


import Language.JSON


import Lists
import IOUtils
import JSONUtils


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


||| Core type to carry environment (context) state.
record Environment where
  ||| Creates an environment (context).
  |||
  ||| @dir        the path of the current working directory
  ||| @equations  a key-value collection of power-law equations
  ||| @groups     a key-value collection of groups of power-law equations
  constructor MkEnvironment
  dir : String
  equations : List (String, Zipf)
  groups : List (String, List (String, Zipf))


||| For a power-law equation, computes y for a given x.
|||
||| @f  the equation
||| @x  the x-value
comp : (f : Zipf) -> (x : Double) -> Double
comp f x = (amp f) * (pow x (alpha f))


||| Computes the average slope of a function between two points.
|||
||| @f  the equation to compute for
||| @a  the starting x-value
||| @b  the ending x-value
slope : (f : Zipf) -> (a : Double) -> (b : Double) -> Double
slope f a b = (abs ((comp f a) - (comp f b))) / (abs (a - b))


||| Adds a Zipf equation to the environment.
|||
||| @env  the environment to add the equation to
||| @name the name to add the equation under
||| @eq   the equation to add
addZipf : (env : Environment) -> (name : String) -> (eq : Zipf) -> Environment
addZipf env name eq = MkEnvironment (dir env) ((name, eq) :: (equations env)) (groups env)


||| Gets a named Zipf equation from an environment.
|||
||| @env  the environment to get the equation from
||| @name the name of the equation to get
getZipf : (env : Environment) -> (name : String) -> Maybe Zipf
getZipf env name = lookup (equations env) name


||| Adds an empty group to an environment.
|||
||| @env  the environment to add the group to
||| @name the name for the new group
addGroup : (env : Environment) -> (name : String) -> Environment
addGroup env name = MkEnvironment (dir env) (equations env) ((name, []) :: (groups env))


||| Gets a group from an envionment by name.
|||
||| @env  the environment to get the group from
||| @name the name of the group to get
getGroup : (env : Environment) -> (name : String) -> Maybe (List (String, Zipf))
getGroup env name = lookup (groups env) name


||| Deletes a group from an environment by name
|||
||| @env  the environment to delete the group from
||| @name the name of the group to delete
removeGroup : (env : Environment) -> (name : String) -> Environment
removeGroup env name = MkEnvironment (dir env) (equations env) (filter (\(a, b) => a /= name) (groups env))



addToGroup : (env : Environment) -> (groupId : String) -> (eq : Zipf) -> (eqId : String) -> Maybe Environment
addToGroup env groupId eq eqId =
  case lookup (groups env) groupId of
    Just group =>
      let newGroup = ((eqId, eq) :: group) in
      Just (MkEnvironment (dir env) (equations env) ((groupId, newGroup) :: (remove (groups env) groupId)))
    _ => Nothing


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


comparePair : (x : (String, Double)) -> (y : (String, Double)) -> Ordering
comparePair (_, x') (_, y') = compare x' y'

printRanked : (group : List (String, Zipf)) -> (a : Double) -> (b : Double) -> IO ()
printRanked group a b =
  let vals = map (\(name, eq) => (name, slope eq a b)) group in
  let sorted = sortBy comparePair vals in
  putStrLn (unwords (map (\(name, val) => name) sorted))


printError : (line : Nat) -> (msg : String) -> IO ()
printError line msg = putStrLn $ "Error on line " ++ cast line ++ ": " ++ msg


||| Evaluates a tokenized line of Skeptic assertion code.
|||
||| @env    the environment to evaluate in
||| @tokens the tokens to evaluate
skepticEvalLineTokens : (env : Environment) -> (tokens : List String) -> (lnum : Nat) -> IO Environment
skepticEvalLineTokens env tokens lnum =
  case tokens of
  ["zipf", amp, alpha, "as", name] =>
    pure (addZipf env name (MkZipf (cast amp) (cast alpha))) -- Declare Zipf equation directly.
  ["load", file, "as", name] => do
    eq <- loadEq (joinPaths (dir env) file)
    case eq of
      Just eq' => pure (addZipf env name eq')
      Nothing => do
        printError lnum $ "Could not load equation at " ++ file
        pure env
  ["group", name] => pure (addGroup env name)
  ["add", eq, "to", gr, "as", name] => do
    case getZipf env eq of
      Just z =>
        case (addToGroup env gr z name) of
          Just env' => pure env'
          _ => do
            printError lnum $ "Group " ++ gr ++ " not found."
            pure env -- Environment unchanged.
      _ => do
        printError lnum $ "Equation " ++ eq ++ " not found."
        pure env -- Environment unchanged.
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
  ["rank", group, "between", a, "and", b] => do
    case getGroup env group of
      Just g =>
        printRanked g (cast a) (cast b)
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
skepticEvalLine : (env : Environment) -> (line : String) -> (lnum : Nat) -> IO Environment
skepticEvalLine env line lnum =
  case unpack line of
    ('#' :: _) => pure env -- Ignore comments.
    _ =>
      case stripEmptyTokens (split (== ' ') line) of -- Split along spaces to create token list.
        [] => pure env -- Ignore blank lines.
        tokens => skepticEvalLineTokens env tokens lnum


||| Evaluates lines of Skeptic assertion code.
|||
||| @env    the envronment to evaluate in
||| @lines  the lines to evaluate
skepticEvalLines : (env : Environment) -> (lines : List String) -> (lnum : Nat) -> IO ()
skepticEvalLines env (line :: lines') lnum = do
  env' <- skepticEvalLine env line lnum -- Transform environment.
  skepticEvalLines env' lines' (lnum + 1)
skepticEvalLines _ [] _ = pure ()


||| Evaluates a file of Skeptic assertion code.
|||
||| @path the lines to evaluate
skepticEvalFile : (path : String) -> IO ()
skepticEvalFile path = do
  absDirPath <- resolveAbsPath (dirName path)
  lines <- loadLines path
  case lines of
    Just lines' => skepticEvalLines (MkEnvironment absDirPath [] []) lines' 1
    Nothing => putStrLn "File read error."
