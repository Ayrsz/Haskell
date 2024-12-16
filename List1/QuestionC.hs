suaviza :: [Float] -> Float -> [Float]
suaviza [] _ = []
suaviza (y:[]) _ = [y]
suaviza (y:z:xyz) x
    |   x == -1 = (y:suaviza (z:xyz) y)
    |   otherwise = (((x+y+z)/3): suaviza (z:xyz) y)



main = do
        lista <- getLine
        print $ suaviza (read lista :: [Float]) (-1) 