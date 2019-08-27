module SkepticLang.PairUtils


%access public export


||| Compares the second element in two pairs.
|||
||| @x  the first pair
||| @y  the second pair
compareSnd : Ord b => (x : (a, b)) -> (y : (a, b)) -> Ordering
compareSnd (_, x') (_, y') = compare x' y'
