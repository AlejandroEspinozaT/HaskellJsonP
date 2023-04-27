import Test.HUnit
import HaskellJsonLib

main :: IO ()
main = do
        testFunctions <- runTestTT testFunctions
        print testFunctions

testFunctions :: Test
testFunctions =
    TestList
    [
        TestCase $
        assertEqual
            "should return the correct tuple with false value"
            (parse parseBool "false")
            (Just (JBool False,"")),
        TestCase $
        assertEqual
            "should return the correct tuple with true value"
            (parse parseBool "true")
            (Just (JBool True,"")),
        TestCase $
        assertEqual
            "should return Nothing"
            (parse parseBool "")
            (Nothing),
        TestCase $
        assertEqual
            "should return the correct tuple"
            (parse parseString "\"a\"")
            (Just (JString "a","")),
        TestCase $
        assertEqual
            "should return Nothing"
            (parse parseString "n")
            (Nothing),
        TestCase $
        assertEqual
            "should return the correct tuple"
            (parse parseNumber "1")
            (Just (JNumber 1.0,"")),
        TestCase $
        assertEqual
            "should return Nothing"
            (parse parseNumber "1a")
            (Nothing),
        TestCase $
        assertEqual
            "should return empty list"
            (Just (JList [],""))
            (parse parseList "[]"),
        TestCase $
        assertEqual
            "should return the correct tuple with JNumber value"
            (parse parseList "[3]")
            (Just (JList [Just (JNumber 3.0)],"")),
        TestCase $
        assertEqual
            "should return the correct tuple with String value"
            (parse parseList "[\"z\"]")
            (Just (JList [Just (JString "z")],"")),
        TestCase $
        assertEqual
            "should return the correct tuple with JBool value"
            (parse parseList "[false]")
            (Just (JList [Just (JBool False)],"")),
        TestCase $
        assertEqual
            "should return the correct values of list of list"
            (parse parseList "[[3]]")
            (Just (JList [Just (JList [Just (JNumber 3.0)])],"")),
        TestCase $
        assertEqual
            "should return Nothing"
            (parse parseList "a")
            (Nothing),
        TestCase $
        assertEqual
            "should return the correct tuple with JNumber value"
            (parse parseObject "{\"test\":2}")
            (Just (JObject [("\"test\"",Just (JNumber 2.0))],"")),
        TestCase $
        assertEqual
            "should return the correct tuple with JString value"
            (parse parseObject "{\"test\":\"a\"}")
            (Just (JObject [("\"test\"",Just (JString "a"))],"")),
        TestCase $
        assertEqual
            "should return the correct tuple with JBool value (false)"
            (parse parseObject "{\"test\":false}")
            (Just (JObject [("\"test\"",Just (JBool False))],"")),
        TestCase $
        assertEqual
            "should return the correct tuple with JBool value (true)"
            (parse parseObject "{\"test\":true}")
            (Just (JObject [("\"test\"",Just (JBool True))],"")),
        TestCase $
        assertEqual
            "should return Nothing"
            (parse parseObject "z")
            (Nothing),
        TestCase $
        assertEqual
            "should return the correct value"
            (parseJson "{\"test4\":\"test\", \"object4\":{\"test2\":14, \"object4-2\":{\"name\":\"ale\",\"object4-3\": {\"boolean\":true,\"age\":1290312,\"lista\": [[4,4],[5,8,4],[true,false],[]]}}}}")
            (Just (JObject [("\"test4\"",Just (JString "test")),("\"object4\"",Just (JObject [("\"test2\"",Just (JNumber 14.0)),("\"object4-2\"",Just (JObject [("\"name\"",Just (JString "ale")),("\"object4-3\"",Just (JObject [("\"boolean\"",Just (JBool True)),("\"age\"",Just (JNumber 1290312.0)),("\"lista\"",Just (JList [Just (JList [Just (JNumber 4.0),Just (JNumber 4.0)]),Just (JList [Just (JNumber 5.0),Just (JNumber 8.0),Just (JNumber 4.0)]),Just (JList [Just (JBool True),Just (JBool False)]),Just (JList [])]))]))]))]))])),
        TestCase $
        assertEqual
            "should return the correct value"
            (parseJson "{\"test3\":\"test\", \"object3\":{\"test2\":11, \"object3-2\":{\"name\":\"ale\",\"numeros\":[[9,1],[7,2]]}}}")
            (Just (JObject [("\"test3\"",Just (JString "test")),("\"object3\"",Just (JObject [("\"test2\"",Just (JNumber 11.0)),("\"object3-2\"",Just (JObject [("\"name\"",Just (JString "ale")),("\"numeros\"",Just (JList [Just (JList [Just (JNumber 9.0),Just (JNumber 1.0)]),Just (JList [Just (JNumber 7.0),Just (JNumber 2.0)])]))]))]))])),
        TestCase $
        assertEqual
            "should return the correct value"
            (parseJson "{\"nombre\":[[[\"orange\"],[\"yellow\"]],[[\"red\"],[true,false]]]}")
            (Just (JObject [("\"nombre\"",Just (JList [Just (JList [Just (JList [Just (JString "orange")]),Just (JList [Just (JString "yellow")])]),Just (JList [Just (JList [Just (JString "red")]),Just (JList [Just (JBool True),Just (JBool False)])])]))])),
        TestCase $
        assertEqual
            "should return Nothing"
            (parseJson "")
            (Nothing)
    ]