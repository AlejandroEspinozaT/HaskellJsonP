import Control.Applicative (Applicative(pure, (<*>)), Alternative (empty, (<|>)))


import Data.Char

data JsonValue = JString String
          | JBool Bool
          | JNumber Double
          | JList [Maybe JsonValue ]
          | JObject [(String, Maybe (JsonValue))]
          | JNil
          deriving (Show, Eq)

data Parser a = Parser {parse :: String -> Maybe (a,String)} 

instance Functor Parser where 
    fmap f (Parser function) = Parser(\x -> case function x of 
        Just(a, xs) -> Just (f a, xs)
        Nothing -> Nothing        
        )

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

parseDouble ::Parser JsonValue 
parseDouble = Parser (\x -> case reads x of
    [] -> Nothing
    [(number, s)] -> Just (JNumber number, s)
    _ -> Nothing)

parseNumber :: Parser JsonValue 
parseNumber = Parser (\x -> if isDigit' x
    then case reads x of
        [] -> Nothing
        [(number, s)] -> Just (JNumber number, s)
        _ -> Nothing
        else Nothing)

parseList:: Parser JsonValue
parseList = Parser(\x -> case x of
    
    _ ->Nothing
    )

instance Applicative Parser where
    pure a = Parser (\x -> Just (a, x))
    (Parser a) <*> (Parser b) = Parser(\input ->  do 
        (a' ,rest) <- a input
        (b' , rest') <- b input
        Just(a' b', rest')
        )

instance Alternative Parser where
    empty = Parser(\_ -> Nothing)
    (Parser a ) <|> (Parser b) = Parser (\input -> a input <|> b input)

getJValue :: String -> Maybe JsonValue
getJValue xs =
  case parse (parseBool <|> parseNumber <|> parseString <|> parseList) xs of
    Just (value, _) -> Just value
    Nothing -> Nothing


isDigit' :: String -> Bool
isDigit' [] = False
isDigit' ('-' : xs) = isDigit' xs
isDigit' [x] = isDigit x
isDigit' (x:xs) = isDigit x && isDigit' xs
---------
isOpenCharList :: String -> Bool
isOpenCharList ('[' : _) = True
isOpenCharList _ = False

trim :: String -> String
trim = dropWhile isSpace

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


-- parse parseBool "false, hola"
-- parse parserString "\"hola\"123"
--parse parseDouble "123, hola"
--