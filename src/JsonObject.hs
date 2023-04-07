module JsonObject(JsonValue(..),NodeName) where

type NodeName = String

data JsonValue = JString String
          | JBool Bool
          | JNumber Double
          | JList [Maybe JsonValue]
          | JObject [(String, Maybe JsonValue)]
          | JNil
          deriving (Show, Eq)

