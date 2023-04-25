import Control.Applicative (Applicative(pure, (<*>)), Alternative (empty, (<|>)))
import Text.Read
import Data.Maybe (catMaybes)
import Data.Char
import Data.Maybe

data JsonValue = JString String
          | JBool Bool
          | JNumber Double
          | JList [Maybe JsonValue ]
          | JObject [(String, Maybe (JsonValue))]
          | JNil
          deriving (Show, Eq)

data Parser a = Parser {parse :: String -> Maybe (a,String)}

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
    then case readMaybe x of
        Nothing -> Nothing
        Just number -> Just (JNumber number, "")
    else Nothing)


parseList :: Parser JsonValue
parseList = Parser $ \input ->
    if isOpenCharList (trim input)
        then case (extractObject' (Just input) '[' ']') of
            Just xs -> case traverse parseListContents (splitAcc' ',' (Just xs) [] 0 0) of
                Just vs -> Just (JList (map Just vs), "")
                Nothing -> Nothing
            Nothing -> Nothing

        else Nothing

parseListContents :: String -> Maybe JsonValue
parseListContents s = getJValue (trim s) >>= Just


parseObject :: Parser JsonValue
parseObject = Parser (\x -> case x of

    _ -> Nothing)



extractObject' :: Maybe String -> Char -> Char -> Maybe String
extractObject' Nothing _ _ = Nothing
extractObject' (Just xs) i f = let ys = dropWhile (/=i) xs 
                                   zs = takeWhile (/=f) (tail ys)
                               in if null zs then Nothing else Just zs

isOpenCharObject :: String -> Bool
isOpenCharObject ('{' : _) = True
isOpenCharObject _ = False


getJValue :: String -> Maybe JsonValue
getJValue xs =
  case parse (parseBool <|> parseNumber <|> parseString <|> parseList <|> parseObject) xs of
    Just (value, _) -> Just value
    Nothing -> Nothing


isDigit' :: String -> Bool
isDigit' [] = False
isDigit' ('-' : xs) = isDigit' xs
isDigit' [x] = isDigit x
isDigit' (x:xs) = isDigit x && isDigit' xs

isOpenCharList :: String -> Bool
isOpenCharList ('[' : _) = True
isOpenCharList _ = False

trim :: String -> String
trim = dropWhile isSpace

buildTuple :: String -> (String, Maybe JsonValue)
buildTuple [] = ("", Just (JString ""))
buildTuple s = let x : y : _ = splitAcc ':' (Just s) [] in (trim x, getJValue (trim y))

splitAcc :: Char -> Maybe String -> String -> [String]
splitAcc _ Nothing [] = []
splitAcc _ Nothing (_ : _) = []
splitAcc c (Just xs) ys = splitAcc' c (Just xs) ys 0 0


splitAcc' :: Char -> Maybe String -> String -> Int -> Int -> [String]
splitAcc' _ Nothing [] _ _ = []
splitAcc' _ (Just []) [] _ _ = []
splitAcc' _ (Just []) ys _ _ = [reverse ys]
splitAcc' c (Just xs) ys bracket brace =
    case xs of
    [] -> [reverse ys]
    x:xs' ->
            case x of
                ',' | bracket == 0 && brace == 0 -> reverse ys : splitAcc' c (Just xs') [] bracket brace
                ':' | bracket == 0 && brace == 0 -> reverse ys : splitAcc' c (Just xs') [] bracket brace
                '[' -> splitAcc' c (Just xs') (x:ys) (bracket + 1) brace
                '{' -> splitAcc' c (Just xs') (x:ys) bracket (brace + 1)
                ']' -> splitAcc' c (Just xs') (x:ys) (bracket - 1) brace
                '}' -> splitAcc' c (Just xs') (x:ys) bracket (brace - 1)
                _ -> splitAcc' c (Just xs') (x:ys) bracket brace
-- parse parseBool "false, hola "
-- parse parserString "\"hola\"123"
--parse parseDouble "123, hola"
--