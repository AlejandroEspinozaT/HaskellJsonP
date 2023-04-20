import Control.Applicative (Applicative(pure, (<*>)))

{-# LANGUAGE LambdaCase #-}

data JsonValue a = JString String
          | JBool Bool
          | JNumber Double
          | JList [Maybe (JsonValue a)]
          | JObject [(String, Maybe (JsonValue a))]
          | JNil
          deriving (Show, Eq)

--transformer -monad-sin monad aplicative- investigar 

data Parser a = Parser {parse :: String -> Maybe (a,String)} --analizador sintactico

instance Functor Parser where 
    fmap f (Parser function) = Parser(\x -> case function x of 
        Just(a, xs) -> Just (f a, xs)
        Nothing -> Nothing        
        )

parseBool :: Parser (JsonValue a) 
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


parseString :: Parser (JsonValue a)
parseString = Parser (\x -> case x of
    "" -> Nothing 
    ('\"':xs) -> Just (JString (takeWhile (/= '\"') xs), drop 1 (dropWhile (/= '\"') xs)) 
    _ -> Nothing)

parseDouble ::Parser (JsonValue a)

parseDouble = Parser (\x -> case reads x of
    [] -> Nothing
    [(number, s)] -> Just (JNumber number, s)
    _ -> Nothing)


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