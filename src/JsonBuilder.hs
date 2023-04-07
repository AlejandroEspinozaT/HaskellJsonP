{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module JsonBuilder where

import JsonObject
import Data.List
import Data.Char
import qualified Data.Maybe

showNode :: (String, Maybe JsonValue) -> String
showNode (name,value) = name ++ ": " ++ writeJson value

createFromObject :: [(String,Maybe JsonValue)] -> String
createFromObject [] = ""
createFromObject xs = intercalate "," (map showNode xs)

writeJson :: Maybe JsonValue -> String
writeJson (Just (JString s)) = s
writeJson (Just (JBool b)) = show b
writeJson (Just (JNumber n)) = show n
writeJson (Just (JList l)) = show (map writeJson l)
writeJson (Just (JObject o)) = "{" ++ createFromObject o ++ "}"

containsList :: String -> Char -> Bool
containsList xs c = isPrefixOf "[" xs && isPrefixOf "]" xs

parseJson :: String -> Maybe JsonValue
parseJson [] = Nothing
parseJson s = Just (JObject (map buildTuple (splitAcc ',' (extractObject' (Just (trim s))) [])))

extractObject :: Maybe String -> Char -> Char -> Maybe String
extractObject Nothing _ _ = Nothing
extractObject (Just(_:xs)) i f = Just (dropWhile (==i) (takeWhile (/=f) xs))

extractObject' :: Maybe String -> Maybe String
extractObject' Nothing = Nothing
extractObject' (Just []) = Nothing
extractObject' (Just (_:xs)) = Just (init xs)

fromMaybe :: Maybe a -> a
fromMaybe Nothing = error "no value"
fromMaybe (Just x) = x

parseString :: String -> Maybe JsonValue
parseString "" = Nothing
parseString ('\"' : xs) = Just (JString (takeWhile (/= '\"') xs))
parseString _ = Nothing

parseBool :: String -> Maybe JsonValue
parseBool ('f' : 'a' : 'l' : 's' : 'e' : xs) = Just (JBool False)
parseBool ('t' : 'r' : 'u' : 'e' : xs) = Just (JBool True)
parseBool _ = Nothing

parseNumber :: String -> Maybe JsonValue
parseNumber n = if isDigit' n then Just (JNumber (read n)) else Nothing

parseList :: String -> Maybe JsonValue
parseList [] = Nothing
parseList xs = if isOpenCharList xs then Just (JList (parseList' xs)) else parseObject xs

parseList' :: String -> [Maybe JsonValue]
parseList' [] = [Just JNil]
parseList' xs = map getJValue (splitAcc ',' (extractObject' (Just xs)) [])

parseObject :: String -> Maybe JsonValue
parseObject [] = Nothing
parseObject xs = Just (JObject (parseObject' xs))

parseObject' :: String -> [(String, Maybe JsonValue)]
parseObject' [] = [("", Just JNil)]
parseObject' xs = if isOpenCharObject xs then map buildTuple (splitAcc ',' (extractObject' (Just xs)) []) else [("",Nothing)]

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' [x] = isDigit x
isDigit' ('-':x:xs) = isDigit x && isDigit' xs
isDigit' (x:xs) = isDigit x && isDigit' xs

isOpenCharList :: String -> Bool
isOpenCharList ('[' : _) = True
isOpenCharList _ = False

isOpenCharObject :: String -> Bool
isOpenCharObject ('{' : _) = True
isOpenCharObject _ = False

splitToTuples :: String -> [(String, Maybe JsonValue)]
splitToTuples s = map buildTuple (splitAcc ',' (Just s) [])


trim :: String -> String
trim = dropWhile isSpace


buildTuple :: String -> (String, Maybe JsonValue)
buildTuple [] = ("", Just (JString ""))
buildTuple s = let x : y : _ = splitAcc ':' (Just s) [] in (trim x, getJValue (trim y))

getJValue :: String -> Maybe JsonValue
getJValue xs
            | Data.Maybe.isJust (parseBool xs) = parseBool xs
            | Data.Maybe.isJust (parseNumber xs) = parseNumber xs
            | Data.Maybe.isJust (parseString xs) = parseString xs
            | Data.Maybe.isJust (parseList xs) = parseList xs
            | Data.Maybe.isJust (parseObject xs) = parseObject xs
            | otherwise = Nothing


splitAcc :: Char -> Maybe String -> String -> [String]
splitAcc _ Nothing [] = []
splitAcc _ Nothing (_ : _) = []
splitAcc c (Just xs) ys = splitAcc' c xs ys 0 0

splitAcc' :: Char -> String -> String -> Int -> Int -> [String]
splitAcc' _ [] [] _ _ = []
splitAcc' _ [] ys _ _ = [reverse ys]
splitAcc' c (x:xs) ys bracket brace
    | x == c && x == ',' && (bracket == 0 && brace == 0) = reverse ys : splitAcc' c xs [] bracket brace
    | x == c && x == ':' && (bracket == 0 && brace == 0) = reverse ys : splitAcc' c xs [] bracket brace
    | x == '[' = splitAcc' c xs (x:ys) (bracket + 1) brace 
    | x == '{' = splitAcc' c xs (x:ys) bracket (brace + 1) 
    | x == ']' = splitAcc' c xs (x:ys) (bracket - 1) brace 
    | x == '}' = splitAcc' c xs (x:ys) bracket (brace - 1) 
    | otherwise = splitAcc' c xs (x:ys) bracket brace

extractNodes :: String -> [(String, String)]
extractNodes xs = map parseKeyValuePairs (splitAcc ',' (Just xs) [])

parseKeyValuePairs :: String -> (String, String)
parseKeyValuePairs s = let x:y:xs = splitAcc ':' (Just s) [] in (x, y)

extractSubList :: Maybe String -> Char -> Char -> Maybe String
extractSubList Nothing _ _ = Nothing
extractSubList (Just x) i f = Just (tail (init x))





