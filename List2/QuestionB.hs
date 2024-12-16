type Quantia = (String, Int)

palavrasFrequentes :: [String] -> [String]
palavrasFrequentes [] = []
palavrasFrequentes str = tres
    where
        vetor = createList str
        ordenado = quickSort vetor
        tres = retornaTres ordenado 0




createList :: [String] -> [Quantia]
createList [] = []
createList (s:str) = [countRepeat s (s:str)] ++ createList (filter (stringsDif s) (s:str))

countRepeat :: String -> [String] -> Quantia
countRepeat palavra vetor = (palavra, length (filter (stringsIguais palavra) vetor))

stringsIguais :: String -> String -> Bool
stringsIguais a b = a == b

stringsDif :: String -> String -> Bool
stringsDif a b = a /= b

quickSort :: [Quantia] -> [Quantia]
quickSort [] = []
quickSort ((a,b):xs) = quickSort [(y,z) | (y,z) <- xs, z > b || ((z == b) && (length y < length a ))] ++ [(a,b)] ++ quickSort [(y,z) | (y,z) <- xs, z < b || ((z == b) && length a < length y)]

retornaTres :: [Quantia] -> Int -> [String]
retornaTres [] _ = []
retornaTres xs 3 = []
retornaTres ((a, b):xs) n = [a] ++ retornaTres xs (n+1)

main = do
        lista <- getLine
        print $ palavrasFrequentes (read lista :: [String])
