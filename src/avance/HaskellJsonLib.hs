module HaskellJsonLib where

import Control.Applicative (Applicative(pure, (<*>)), Alternative (empty, (<|>)))
import Text.Read
import Data.Char
import Data.List

data JsonValue = JString String
          | JBool Bool
          | JNumber Double
          | JList [Maybe JsonValue ]
          | JObject [(String, Maybe (JsonValue))]    
          | JNil
          deriving (Show, Eq)

data Parser a = Parser {parse :: String -> Maybe (a,String)}

writeJson :: Maybe JsonValue -> String
writeJson (Just (JString s)) = s
writeJson (Just (JBool b)) = show b
writeJson (Just (JNumber n)) = show n
writeJson (Just (JList l)) = show (map writeJson l)
writeJson (Just (JObject o)) = "{" ++ createFromObject o ++ "}"

parseJson :: String -> Maybe JsonValue
parseJson [] = Nothing
parseJson s = Just (JObject (map buildTuple (splitAcc' ',' (extractObject' (Just s)) [] False 0)))

showNode :: (String, Maybe JsonValue) -> String
showNode (name,value) = name ++ ": " ++ writeJson value

createFromObject :: [(String,Maybe JsonValue)] -> String
createFromObject [] = ""
createFromObject xs = intercalate "," (map showNode xs)

instance Functor Parser where
    fmap f (Parser function) = Parser (\x -> case function x of
        Just(a, xs) -> Just (f a, xs)
        Nothing -> Nothing
        )
instance Applicative Parser where
    pure a = Parser (\x -> Just (a, x))
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser a) <*> (Parser b) = Parser (\input ->  do
        (a' ,rest) <- a input
        (b' , rest') <- b input
        Just (a' b', rest')
        )

instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    (Parser a ) <|> (Parser b) = Parser (\input -> a input <|> b input)


getJValue :: String -> Maybe JsonValue
getJValue xs =
  case parse (parseBool <|> parseNumber <|> parseString <|> parseList <|> parseObject) xs of
    Just (value, _) -> Just value
    Nothing -> Nothing


parseBool :: Parser JsonValue
parseBool = Parser (\x->case x of
    'f' : 'a' : 'l' : 's' : 'e' : xs -> Just (JBool False,xs)
    't' : 'r' : 'u' : 'e' : xs -> Just (JBool True,xs)
    _ ->Nothing)


parseString :: Parser JsonValue
parseString = Parser (\x -> case x of
    "" -> Nothing
    ('\"':xs) -> Just (JString (takeWhile (/= '\"') xs), drop 1 (dropWhile (/= '\"') xs))
    _ -> Nothing)


parseNumber :: Parser JsonValue
parseNumber = Parser (\x -> if isDigit' x
    then case readMaybe x of
        Nothing -> Nothing
        Just number -> Just (JNumber number, "")
    else Nothing)

parseList :: Parser JsonValue
parseList = Parser $ \input ->
    if isOpenCharList (trim input)
        then case (extractObject' (Just input)) of
            Just xs -> case traverseN parseList' (splitAcc' ',' (Just xs) [] False 0) of
                Just vs -> Just (JList (map Just vs), "")
                Nothing -> Nothing
            Nothing -> Nothing
        else Nothing
        
parseList' :: String -> Maybe JsonValue
parseList' s = getJValue (trim s) 

traverseN :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseN _ [] = Just []
traverseN f (x:xs) = case f x of
                        Just y -> case traverseN f xs of
                                    Just ys -> Just (y:ys)
                                    Nothing -> Nothing
                        Nothing -> Nothing


parseObject :: Parser JsonValue
parseObject = Parser $ \input ->
  if isOpenCharObject (trim input)
    then case extractObject' (Just input)  of
           Just objectString -> case parseObject' objectString of
                                  Just (jobj, rest) -> Just (JObject jobj, rest)
                                  _ -> Nothing
           Nothing -> Nothing
    else Nothing

parseObject' :: String -> Maybe ([(String, Maybe JsonValue)], String)
parseObject' input = case splitAcc' ',' (Just input) [] False 0 of
                      [] -> Just ([], "")
                      segments -> parseJsonObj segments []
  where
    parseJsonObj [] acc = Just (reverse acc, "")
    parseJsonObj (segment:segments) acc =
      case splitAcc' ':' (Just segment) [] False 0 of
        [key, value] -> case (trim key, getJValue (trim value)) of
                          ("", _) -> Nothing
                          (_, Nothing) -> Nothing
                          (parsedKey, Just parsedValue) -> parseJsonObj segments ((parsedKey, Just parsedValue) : acc)
        _ -> Nothing

buildTuple :: String -> (String, Maybe JsonValue)
buildTuple [] = ("", Just (JString ""))
buildTuple s = let x:y:xs = splitAcc' ':' (Just (trim s)) [] False 0 in (trim x, getJValue (trim y))


extractObject' :: Maybe String -> Maybe String
extractObject' Nothing = Nothing
extractObject' (Just []) = Nothing
extractObject' (Just (_:xs)) = Just (init xs)

isOpenCharObject :: String -> Bool
isOpenCharObject ('{' : _) = True
isOpenCharObject _ = False

isOpenCharList :: String -> Bool
isOpenCharList ('[' : _) = True
isOpenCharList _ = False

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' ('-' : xs) = isDigit' xs
isDigit' [x] = isDigit x
isDigit' (x:xs) = isDigit x && isDigit' xs

trim :: String -> String
trim = dropWhile isSpace

splitAcc' :: Char -> Maybe String -> String -> Bool -> Int -> [String]
splitAcc' _ Nothing [] _ _ = []
splitAcc' _ (Just []) [] _ _ = []
splitAcc' _ (Just []) ys _ _ = [reverse ys]
splitAcc' c (Just (x:xs)) ys inBracket count
  | x == c && not inBracket = reverse ys : splitAcc' c (Just xs) [] False count
  | x == '[' || x == '{' = splitAcc' c (Just xs) (x:ys) True (count + 1)  
  | x == ']' || x == '}' = splitAcc' c (Just xs) (x:ys) (count > 1) (count - 1)
  | otherwise = splitAcc' c (Just xs) (x:ys) inBracket count
