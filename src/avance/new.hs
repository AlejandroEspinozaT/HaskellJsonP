import Control.Applicative (Applicative(pure, (<*>)), Alternative (empty, (<|>)))
import Data.List
import Data.Char
import Control.Monad
{-# LANGUAGE LambdaCase #-}

data JsonValue = JString String
          | JBool Bool
          | JNumber Double
          | JList [Maybe JsonValue ]
          | JObject [(String, Maybe (JsonValue))]
          | JNil
          deriving (Show, Eq)

--transformer -monad-sin monad aplicative- investigar 

data Parser a = Parser {parse :: String -> Maybe (a,String)} --analizador sintactico

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

{-
parseString :: String -> Maybe JsonValue
parseString "" = Nothing
parseString ('\"' : xs) = Just (JString (takeWhile (/= '\"') xs))
parseString _ = Nothing

----
parserString :: Parser String
parserString = Parser(\x -> case x of
    "" -> Nothing
    ('\"':xs) -> Just (takeWhile (/= '\"') xs, drop 1 (dropWhile (/= '\"') xs))
    _ -> Nothing)

-}
------------------
parseList:: Parser JsonValue
parseList = Parser(\x -> case x of
    _ ->Nothing
    )




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

{- Listas 

parseList :: String -> Maybe JsonValue
parseList [] = Nothing
parseList xs = if isOpenCharList xs then Just (JList (parseList' xs)) else parseObject xs

parseList' :: String -> [Maybe JsonValue]
parseList' [] = [Just JNil]
parseList' xs = map getJValue (splitAcc ',' (extractObject' (Just xs)) [])

(se puede hacer con <|>)
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

parseObject :: String -> Maybe JsonValue
parseObject [] = Nothing
parseObject xs = Just (JObject (parseObject' xs))

parseObject' :: String -> [(String, Maybe JsonValue)]
parseObject' [] = [("", Just JNil)]
parseObject' xs = if isOpenCharObject xs then map buildTuple (splitAcc ',' (extractObject' (Just xs)) []) else [("",Nothing)]


-}
-------------------


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

-- parse parseBool "false, hola"
-- parse parserString "\"hola\"123"
--parse parseDouble "123, hola"

{-

parseNumber :: Parser (JsonValue a)
parseNumber = Parser (\x -> case reads x of
    [(num, "")] -> Just (JNumber num, "")
    _ -> Nothing)






(\x -> case readMaybe x of
                             Just num -> Just (JNumber num, "")
                             Nothing -> Nothing)
parseNumber :: String -> Maybe JsonValue
parseNumber n = if isDigit' n then Just (JNumber (read n)) else Nothing

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' [x] = isDigit x
isDigit' ('-':x:xs) = isDigit x && isDigit' xs
isDigit' (x:xs) = isDigit x && isDigit' xs
-}
-- parse parseBool "false, hola"
-- parse parserString "\"hola\"123"
--parse parseDouble "123, hola"


sumOfTree :: Maybe Int
sumOfTree = pure (\x y z -> x+y+z) <*> Just 3 <*> Just 4 <*> Just 5

{-
instance Applicative Maybe where
    pure:: a -> Maybe a
    pure x = Just x
    pure _ = Nothing
    -- <*>:: Maybe(a->b)-> Maybe a ->Maybe b 
    (<*>) Nothing _ = Nothing
    (<*>) (Just f) (Just a) = Just(f a)
-}


{-
instance Applicative Maybe where
    pure:: a -> Maybe a
    pure x = Just x
    pure _ = Nothing
    -- <*>:: Maybe(a->b)-> Maybe a ->Maybe b 
    (<*>) Nothing _ = Nothing
    (<*>) (Just f) (Just a) = Just(f a)
--function :: String -> Maybe (a,String)
--function 

instance Applicative [] where
    pure :: a -> [a]
    pure a = [a]
    (<*>) [] _ = []
    (<*>) _ [] = []
-}

{-
Reglas de aplicative !!
1 Indetity
2 Composition 
3 

-}
sum2 :: Int -> Int->Int -> Int
sum2 x y z = x+y+z

data Person = Person {name::String,age::Int}deriving(Show)

validateNameLength:: String -> Int -> Maybe String
validateNameLength name age = if length name <= 50
    then Just name 
    else Nothing 

createName :: String -> Maybe String 
createName name = validateNameLength name 50
{-
fmap2 :: (a->b->c) = fa-> fb ->fc
fmap2 f a b = pure f <*> a <*> b
fmap3 f a b c = pure f <*> a <*> b <*>c

---------------


-}
instance Applicative Parser where
    --pure a -> Parser
   -- pure:: a -> f a
    pure a = Parser (\x -> Just (a, x))
    --(<*>) :: f(a->b) -> f a->f b 
    (Parser a) <*> (Parser b) = Parser(\input ->  do 
        (a' ,rest) <- a input
        (b' , rest') <- b input
        Just(a' b', rest')
        )

--Alternative
instance Alternative Parser where
    --empty
    -- <|>
    empty = Parser(\_ -> Nothing)
    (Parser a ) <|> (Parser b) = Parser (\input -> a input <|> b input)


--Graham hutton 