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

getMinMax :: [String] -> (Double, Double) -> (Double, Double)
getMinMax [] (min, max) = (min, max)
getMinMax (x:xs) (min, max)
    | ehNum x && (numero > max) = getMinMax xs (min, numero)
    | ehNum x && (numero < min) = getMinMax xs (numero, max)
    | otherwise = getMinMax xs (min, max)
    where
        numero = returnNum x

main = do
    a <- getLine
    let positions = returnPos a 0
    let subtrings = returnSubstrings a [] 0 positions
    let result = getMinMax subtrings (31293123, 0)
    print result