module Dictionary where

cleanString :: String -> String
cleanString = filter (\c -> c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \"")