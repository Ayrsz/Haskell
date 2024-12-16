
digitToInt :: Char -> Int
digitToInt c = fromEnum(c) - fromEnum('0')

multply :: [Char] -> Int
multply [] = 1
multply (x:xs) = digitToInt(x) * multply xs

luta :: String -> String
luta [x, y, z, w]
    |(ab - cd)^2 < r = "Charmander vitorioso"
    |otherwise = "Charmander derrotado"
    where
        ab = multply [x, y]
        cd = multply [z, w]
        r = multply [x, y, z, w]

main = do
    abcd <- getLine 
    let result = luta abcd
    print result