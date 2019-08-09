||| Loads lines from a text file.
||| @path   the path to the file
load_lines : (path : String) -> IO (Maybe (List String))
load_lines path = do
  txt <- readFile path
  case txt of
    Right txt' => pure (Just (lines txt'))
    _ => pure Nothing


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


||| A collection of cases for a fitlered dataset.
public export
record CaseCollection where
  ||| Creates a case collection.
  |||
  ||| @worst    the equation representing the worst-case distribution
  ||| @average  the equation representing the average-case distribution
  ||| @best     the equation representing the best-case distribution
  constructor MkCaseCollection
  worst : Zipf
  average : Zipf
  best : Zipf


record Environment where
  constructor MkEnvironment
  equations : List (String, Zipf)
  caseCollections : List (String, CaseCollection)


comp : (f : Zipf) -> (x : Double) -> Double
comp f x = (amp f) * (pow x (alpha f))


||| Computes the average slope of a function between two points.
||| ...
slope : (f : Zipf) -> (a : Double) -> (b : Double) -> Double
slope f a b = (abs ((comp f a) - (comp f b))) / (abs (a - b))


||| If slope is shallower, the distribution is more uniform so the policy is better.
public export
better : Double -> Double -> Bool
better x y = x < y


||| If slope is steeper, the distribution is less uniform so the policy is worse.
public export
worse : Double -> Double -> Bool
worse x y = x > y


addZipf : (env : Environment) -> (name : String) -> (eq : Zipf) -> Environment
addZipf env name eq = MkEnvironment ((name, eq) :: (equations env)) (caseCollections env)


getZipf : (env : Environment) -> (name : String) -> Maybe Zipf
getZipf env name = getZipf' (equations env) name where
  getZipf' : (eqs : List (String, Zipf)) -> (nn : String) -> Maybe Zipf
  getZipf' [] _ = Nothing
  getZipf' ((nm, eq) :: eqs') nn = if nm == nn then (Just eq) else getZipf' eqs' nn


skepticEvalLine : (env : Environment) -> (line : String) -> IO (Environment)
skepticEvalLine env line = case split (== ' ') line of
  ["zipf", amp, alpha, "as", name] => do -- Declare Zipf equation directly.
    pure (addZipf env name (MkZipf (cast amp) (cast alpha)))
  ["assert", x, "shallower", y, "between", a, "and", b] => do
    case (getZipf env x, getZipf env y) of
      (Just x', Just y') =>
        if slope x' (cast a) (cast b) > slope y' (cast a) (cast b) then
          putStrLn "Could not assert this."
        else
          putStrLn "Successfully asserted this."
      _ => putStrLn "Equation error."
    pure env
  _ => do
    putStrLn "Parse error."
    pure env


skepticEvalLines : (env : Environment) -> (lines : List String) -> IO ()
skepticEvalLines env (line :: lines') = do
  env' <- skepticEvalLine env line
  skepticEvalLines env' lines'
skepticEvalLines _ [] = pure ()


skepticEvalFile : (file : String) -> IO ()
skepticEvalFile file = do
  lines <- load_lines file
  case lines of
    Just lines' => skepticEvalLines (MkEnvironment [] []) lines'
    Nothing => putStrLn "File read error."
