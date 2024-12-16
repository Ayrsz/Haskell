somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos (x:[]) b = [soma x b]
somarMultiplos (x:xs) b = (soma x b : somarMultiplos xs b)

soma :: Int -> Int -> Int
soma num base
    | (num < base || base == 0) = 0
    | mod num base == 0 = num + (soma (num-1) base)
    | otherwise = 0 + (soma (num-1) base)


main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result