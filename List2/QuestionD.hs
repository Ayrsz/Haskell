import Data.Char
type Quantia = (String, Int)

quickSort :: Ord t => [t] -> [t]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs,  x > y ] ++ [x] ++ quickSort [y | y <- xs, x < y ] -- Ordem lexicográfica

minusc :: Char -> Char 
minusc s
    | (s <= 'z' && s >= 'a') || (s == ' ') || (s <= '9' && s >= '0') = s
    | otherwise = chr (ord s - offset)
    where
        offset = ord 'A' - ord 'a'

normalizaLowCase :: String -> String
normalizaLowCase str = map minusc str

separa :: String -> String -> [String]
separa xs ys =  words (normalizaLowCase xs) ++ words (normalizaLowCase ys)

countRepeat :: [String] -> [Quantia]
countRepeat [] = []
countRepeat (s:str) = [(s, length vetorComAPalavra)] ++ countRepeat (vetorSemAPalavra)
    where
        vetorComAPalavra = filter (search s) (s:str)
        vetorSemAPalavra = filter (searchnot s) (s:str)

search :: String -> String -> Bool
search x y = x == y

searchnot :: String -> String -> Bool
searchnot x y = x /= y

uncommonFromTwoSentences :: [Quantia] -> [String]
uncommonFromTwoSentences [] = []
uncommonFromTwoSentences ((s, n):xs)
    | n >= 2 = uncommonFromTwoSentences xs
    | n == 1 = [s] ++ uncommonFromTwoSentences xs


main = do
    sentence_1 <- getLine
    sentence_2 <- getLine
    let result = separa sentence_1 sentence_2
    let a = countRepeat result
    let b = quickSort (uncommonFromTwoSentences a)
    print b
