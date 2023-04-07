module Lib(someFunc) where
import JsonObject
import JsonBuilder
jsonValue = JObject[("daysOfWeek", Just (JList [Just (JString "M"), Just (JString "T"), Just (JString "T")])), ("name", Just (JString "John")), ("lastName", Just (JString "Doe"))]
someFunc :: IO ()
someFunc = print (parseJson ejem9)
ejem = "{\"nombre\":\"Ale\", \"boolean\":true, \"age\":19}"
ejem2 = "{\"numero\":1, \"object\":{\"age\":2}}"
ejem3 = "{\"favoriteColors\":[2,3,3]}"
ejem4 = "{\"favoriteColors\":[[\"orange\",\"yellow\",\"red\"],[\"orange\",\"yellow\",\"red\"],[\"orange\",\"yellow\",\"red\"]]}"
ejem5 = "{\"nombre\":[[[\"orange\"],[\"yellow\"]],[[\"red\"],[true,false]]]}"
ejem6 = "{\"test1\":\"test\", \"object1\":{\"number\":7}}"
ejem7 = "{\"test2\":\"test\", \"object2\":{\"test2\":77, \"object2-2\":{\"name\":\"ale\",\"boolean\":true}}}"
ejem8 = "{\"test3\":\"test\", \"object3\":{\"test2\":11, \"object3-2\":{\"name\":\"ale\",\"numeros\":[[9,1],[7,2]]}}}"
ejem9 = "{\"test4\":\"test\", \"object4\":{\"test2\":14, \"object4-2\":{\"name\":\"ale\",\"object4-3\": {\"boolean\":true,\"age\":1290312,\"lista\": [[4,4],[5,8,4],[true,false],[]]}}}}"
