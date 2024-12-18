data Animal = Cisnal | Iguanoide | Narvale | Null
  deriving (Eq, Show)


achaAnimal :: [Int] -> Float -> [Int]
achaAnimal (x:y:z:s) i
    | i >= 0.8 = [x] ++ [y] ++ [z+1]
    | i >= 0.4 && i <= 0.7 = [x] ++ [y+1] ++ [z]
    | i >= 0.1 && i <= 0.3 = [x+1] ++ [y] ++ [z]
    | otherwise = (x:y:z:s)

comparaCarac :: String -> String -> Float
comparaCarac strx []  = 0
comparaCarac [] stry  = 0
comparaCarac (cx:strx) (cy:stry)
    | cx == cy = 1.0 + comparaCarac strx stry
    | otherwise = 0.0 + comparaCarac strx stry

stringMaior :: String -> String -> Int
stringMaior x y = max (length x) (length y)

returnI :: Float -> Int -> Float
returnI soma maior = soma / (fromIntegral maior :: Float)

dna2 :: [String] -> [String] -> [Int] -> [Int]
dna2 [] b cIN = cIN
dna2 a [] cIN = cIN
dna2 (l1:lista1) (l2:lista2) cIN = dna2 lista1 lista2 newcIN
    where
        maior = stringMaior l1 l2
        soma = comparaCarac l1 l2
        i = returnI soma maior
        newcIN = achaAnimal cIN i

    





main = do
  firstExtract <- words <$> getLine                       -- equivalente a (read firstExtract :: [String])
  secondExtract <- words <$> getLine
  let result = dna2 firstExtract secondExtract [0, 0, 0]
  print result