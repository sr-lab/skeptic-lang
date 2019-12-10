module SkepticLang.Main


import Language.JSON


import ListUtils
import PairUtils
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


||| Represents a named power-law equation.
public export
NamedZipf : Type
NamedZipf = (String, Zipf)


||| Represents a group of named power-law equations.
public export
Group : Type
Group = List NamedZipf


||| Represents a named group of named power-law equations.
public export
NamedGroup : Type
NamedGroup = (String, Group)


||| Core type to carry environment (context) state.
public export
record Environment where
  ||| Creates an environment (context).
  |||
  ||| @dir        the path of the current working directory
  ||| @equations  a key-value collection of power-law equations
  ||| @groups     a key-value collection of groups of power-law equations
  constructor MkEnvironment
  dir : String
  equations : List NamedZipf
  groups : List NamedGroup


||| Represents a named slope calculated from a named power-law equation.
public export
NamedSlope : Type
NamedSlope = (String, Double)


||| For a power-law equation, computes y for a given x.
|||
||| @f  the equation
||| @x  the x-value
comp : (f : Zipf) -> (x : Double) -> Double
comp f x = (amp f) * (pow x (alpha f))


||| Computes the average slope of a function between two points.
|||
||| @a  the starting x-value
||| @b  the ending x-value
||| @z  the equation to compute for
slope : (a : Double) -> (b : Double) -> (z : Zipf) -> Double
slope a b z = (abs ((comp z a) - (comp z b))) / (abs (a - b))


||| Computes a named average slope of a named function between two points.
|||
||| @a  the starting x-value
||| @b  the ending x-value
||| @z  the named equation to compute for
namedSlope : (a : Double) -> (b : Double) -> (z : NamedZipf) -> NamedSlope
namedSlope a b (n, f) = (n, slope a b f)


||| Adds a Zipf equation to the environment.
|||
||| @env  the environment to add the equation to
||| @name the name to add the equation under
||| @eq   the equation to add
addZipf : (env : Environment) -> (name : String) -> (eq : Zipf) -> Environment
addZipf env name eq = MkEnvironment (dir env) ((name, eq) :: (equations env)) (groups env)


||| Gets a named Zipf equation from an environment by name.
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
getGroup : (env : Environment) -> (name : String) -> Maybe Group
getGroup env name = lookup (groups env) name


||| Adds a named equation to a named group in an environment.
|||
||| @env        the environment to add the equation to
||| @groupName  the name of the group to add the equation to
||| @eq         the equation to add to the group
||| @eqName     the name within the group to bind the equation to
addToGroup : (env : Environment) -> (groupName : String) -> (eq : Zipf) -> (eqName : String) -> Maybe Environment
addToGroup env groupName eq eqName =
  case lookup (groups env) groupName of
    Just group =>
      let newGroup = ((eqName, eq) :: group) in
      Just (MkEnvironment (dir env) (equations env) ((groupName, newGroup) :: (groups env)))
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
      _ => pure Nothing
    _ => pure Nothing


||| Returns a function defining a relation between numbers from a name.
|||
||| @name the name to use for the lookup
getRelation : (name : String) -> Maybe (Double -> Double -> Bool)
getRelation name = case name of
  "shallower" => Just (<)
  "steeper" => Just (>)
  _ => Nothing


||| Ranks equations in a group.
|||
||| @group  the group to rank
||| @a      the starting guess number
||| @b      the ending guess number
printRanked : (group : Group) -> (a : Double) -> (b : Double) -> IO ()
printRanked group a b =
  let vals = map (namedSlope a b) group in -- Equations to values.
  let sorted = sortBy compareSnd vals in -- Sort pairs by values.
  putStrLn (unwords (map (\(name, val) => name ++ ": " ++ cast val ++ "\n") sorted)) -- TODO: Prettier printing.


printRankedAlpha : (group : Group) -> IO ()
printRankedAlpha group =
  let sorted = sortBy (\a, b => compare (alpha (snd a)) (alpha (snd b))) group in -- Sort pairs by values.
  putStrLn (unwords (map (\(name, val) => name) sorted)) -- TODO: Prettier printing.


||| Prints an error message.
|||
||| @line the line number at which the error occurred
||| @msg  the message to show
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
    pure $ addZipf env name $ MkZipf (cast amp) (cast alpha) -- Declare Zipf equation directly.
  ["load", file, "as", name] => do
    eq <- loadEq $ joinPaths (dir env) file
    case eq of
      Just eq' => pure $ addZipf env name eq'
      Nothing => do
        printError lnum $ "Could not load equation file at " ++ file
        pure env
  ["group", name] =>
    pure $ addGroup env name
  ["add", eq, "to", group, "as", name] => do
    case getZipf env eq of
      Just eq' =>
        case addToGroup env group eq' name of
          Just env' =>
            pure env' -- Environment changed.
          _ => do
            printError lnum $ "Group " ++ group ++ " not found."
            pure env -- Environment unchanged.
      _ => do
        printError lnum $ "Equation " ++ eq ++ " not found."
        pure env -- Environment unchanged.
  ["assert", x, r, y] => do
    case (getZipf env x, getZipf env y, getRelation r) of -- Look up equations.
      (Just x', Just y', Just r') =>
        if r' (alpha x') (alpha y') then
          printError lnum $ unwords ["Failed to assert that", x, "is", r, "than", y]
        else
          pure () -- No output means success.
      _ => putStrLn "Equation error."
    pure env
  ["assert", bx, x, r, by, y] => do
    case (getGroup env x, getGroup env y, getRelation r) of -- Look up equations.
      (Just x', Just y', Just r') =>
        case (lookup x' bx, lookup y' by) of
          (Just xx', Just yy') =>
            if r' (alpha xx') (alpha yy') then
              printError lnum $ unwords ["Failed to assert that", bx, x, "is", r, "than", by, y]
            else
              pure () -- No output means success.
          _ => putStrLn "ERR" -- TODO: Opportunity for better error messages.
      _ => putStrLn "Equation error."
    pure env
  ["rank", group] => do
    case getGroup env group of
      Just group' =>
        printRankedAlpha group'
      _ =>
        printError lnum $ "No such group as " ++ group
    pure env
  "say" :: tokens' => do
    putStrLn (unwords tokens')
    pure env
  _ => do
    printError lnum $ "Failed to parse line."
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
||| @lineNum  the current line number for reporting purposes
skepticEvalLine : (env : Environment) -> (line : String) -> (lineNum : Nat) -> IO Environment
skepticEvalLine env line lineNum =
  case unpack line of
    ('#' :: _) => pure env -- Ignore comments.
    _ =>
      case stripEmptyTokens (split (== ' ') line) of -- Split along spaces to create token list.
        [] => pure env -- Ignore blank lines.
        tokens => skepticEvalLineTokens env tokens lineNum -- Run line.


||| Evaluates lines of Skeptic assertion code.
|||
||| @env      the envronment to evaluate in
||| @lines    the lines to evaluate
||| @lineNum  the current line number for reporting purposes
skepticEvalLines : (env : Environment) -> (lines : List String) -> (lineNum : Nat) -> IO ()
skepticEvalLines env (line :: lines') lineNum = do
  env' <- skepticEvalLine env line lineNum -- Transform environment with current line.
  skepticEvalLines env' lines' $ lineNum + 1 -- Next line.
skepticEvalLines _ [] _ = pure ()


||| Evaluates a file of Skeptic assertion code.
|||
||| @path the lines to evaluate
skepticEvalFile : (path : String) -> IO ()
skepticEvalFile path = do
  absDirPath <- resolveAbsPath (dirName path) -- Absolute directory path needed.
  lines <- loadLines path -- Load entire source file.
  case lines of
    Just lines' => skepticEvalLines (MkEnvironment absDirPath [] []) lines' 1 -- New environment, start line 1.
    Nothing => putStrLn $ "Error: Could not read file at " ++ path -- File read error.
