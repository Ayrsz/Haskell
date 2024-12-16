import Data.List (isPrefixOf, isSuffixOf)

returnNum :: String -> Double
returnNum s = read s 

ehNum :: String -> Bool
ehNum [] = False
ehNum (s:str)
    | (s >= '0' && s <= '9') && str == [] = True
    | (s >= '0' && s <= '9') || s == '.' = ehNum str
    | otherwise = False


returnPos :: String -> Int -> [Int]
returnPos [] _ = []
returnPos (x:xs) it
    | x == ';' = (it:returnPos xs (it+1))
    | otherwise = returnPos xs (it+1) 

returnSubstrings :: String -> String -> Int -> [Int] -> [String]
returnSubstrings [] substr _ _ = [substr]
returnSubstrings (x:xs) substr it (loc:locs)
    | it < loc = returnSubstrings xs (substr ++ [x]) (it+1) (loc:locs) 
    | it == loc = (substr: returnSubstrings xs [] (it+1) locs) 

getFatura :: [String] -> String -> Bool -> Double
getFatura [] _ _ = 0
getFatura (x:xs) mes ehmes 
    | mes `isSuffixOf` x = getFatura xs mes True
    | (ehNum x) && ehmes == True = returnNum x + getFatura xs mes False
    | otherwise = getFatura xs mes ehmes

main = do
    mesFatura <- getLine
    a <- getLine
    let positions = returnPos a 0
    let subtrings = returnSubstrings a [] 0 positions
    let result = getFatura subtrings mesFatura False
    print result