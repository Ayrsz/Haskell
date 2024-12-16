btoi :: String -> Int -> Int
btoi (x:[]) _
    | x == '0' = 0
    | x == '1' = 1
btoi (x:xs) m
    | x == '1' = (2^(m-1)) + btoi xs (m-1)
    | x == '0' = 0 + btoi xs (m-1)

size :: String -> Int
size [] = 0
size (x:xs) = 1 + size xs

main = do
    s <- getLine
    let len = size s
    let result = btoi s len
    print result