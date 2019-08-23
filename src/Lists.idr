module SkepticLang.Lists


%access public export


||| Looks up a value by key in a list of key-value pairs.
|||
||| @pairs  the list of key-value pairs
||| @key    the key to look up
lookup : Eq a => (pairs : List (a, b)) -> (key : a) -> Maybe b
lookup [] _ = Nothing
lookup ((a, b) :: pairs') key = if a == key then Just b else lookup pairs' key


||| Removes an entry from a list of key-value pairs, if an entry with that key exists.
|||
||| @pairs  the list of key-value pairs
||| @key    the key of the item to remove
rmId : Eq a => (pairs : List (a, b)) -> (key : a) -> List (a, b)
rmId [] _ = []
rmId ((a, b) :: pairs') key = if a == key then rmId pairs' key else (a, b) :: rmId pairs' key
