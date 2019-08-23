module SkepticLang.JSON


%access public export


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
