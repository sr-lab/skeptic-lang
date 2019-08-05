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


comp : Zipf -> Double -> Double
comp f x = (amp f) * (pow x (alpha f))


||| Computes the average slope of a function between two points.
||| ...
slope : Zipf -> Double -> Double -> Double
slope f a b = (abs ((comp f a) - (comp f b))) / (abs (a - b))

skeptic_exec : String -> IO ()
skeptic_exec line = case split (== ' ') line of
  ["NAH"] => putStrLn "YEAH NAH"
  _ => putStrLn "PERR"

ex' : (lines : List String) -> IO ()
ex' (l :: lines') = do
  skeptic_exec l
  ex' lines'
ex' [] = pure ()

ex : String -> IO ()
ex f = do
  lines <- load_lines f
  case lines of
    Just lines' => ex' lines'
    Nothing => putStrLn "FERR"

||| If slope is shallower, the distribution is more uniform so the policy is better.
public export
better : Double -> Double -> Bool
better x y = x < y


||| If slope is steeper, the distribution is less uniform so the policy is worse.
public export
worse : Double -> Double -> Bool
worse x y = x > y


-- THIS CODE SHOULD BE GENERATED.

public export
rockyou : String -> CaseCollection
rockyou s = case s of
  "basic8" => MkCaseCollection (MkZipf 2 2) (MkZipf 3 3) (MkZipf 1 1)
  "basic16" => MkCaseCollection (MkZipf 2 2) (MkZipf 4 3) (MkZipf 4 5)





-- syntax from [r] assuming [c] at [g] guesses [x] is [o] than [y] =
--   (o (trapz (comp (c (r x))) 1 0 g) (trapz (comp (c (r y))) 1 0 g));


-- tst : (6.0 - 1.0) = (4.0 + 1.0)
-- tst = Refl
--
-- yielder : True = True
-- yielder = ((from rockyou assuming average at 2 guesses "basic8" is better than "basic16") = True)

-- ff : fafafa rockyou assuming average at 100 guesses "basic8" is worse than "basic16"
-- ff = ?ffd

-- ff : Bool
-- ff = lesser (trapz (comp (average (rockyou "basic16"))) 1 0 100) (trapz (comp (average (rockyou "basic16"))) 1 0 100)

-- syntax "assert" [inequality] [c] "uniformity induced in" [d] "by" [x] "than" [y] =
--   ((inequality (trapz (comp (c (d x))) 1 0 100) (trapz (comp (c (d y))) 1 0 100)) = True);
--
-- ggwp : assert greater average uniformity induced in rockyou by "basic8" than "basic16"

--
-- ff : assert greater worst uniformity induced on rockyou by basic8 than basic16
-- ff = ?eh

-- Likewise ordering.
-- assert [greater|lesser] [worst|best|average] uniformity induced on [data] by [policy] than [policy]
-- likewise_[1]_[2]_[3]_[4] : ([1] ([2] [3]) ([2] [4])) = true

-- syntax equal [x] [y] = (x = y);
-- syntax greater [x] [y] = (x > y = True);
--
-- libb : greater 2 1
-- libb = Refl

-- syntax assert [ineq] [c] uniformity induced by [x] than [y] =
--   likewise_ineq_c_x_y : ([1] ([2] [3]) ([2] [4])) = true

-- Comparative ordering.
-- assert [greater|lesser] uniformity induced by [worst|best|average] [policy] than [worst|best|average] [policy]
