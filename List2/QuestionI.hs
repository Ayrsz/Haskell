data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

returnAcid :: Int -> Char
returnAcid n
    |n `mod` 5 == 0 = 'E'
    |n `mod` 5 == 1 = 'M'
    |n `mod` 5 == 2 = 'A'
    |n `mod` 5 == 3 = 'C'
    |n `mod` 5 == 4 = 'S'
--
returnInOrder :: Tree Int -> [Int]
returnInOrder Nilt = []
returnInOrder (Node k (a) (b)) = returnInOrder a ++ [k] ++ returnInOrder b
---
stopinEight :: [Int] -> Int -> [Int] -> ([Int],[Int])
stopinEight [] n list = (list,[])
stopinEight xs 8 list = (list, xs)
stopinEight (x:xs) n list = stopinEight (xs) (n+1) (list++[x]) 

returnMatrixEight :: [Int] -> [[Int]]
returnMatrixEight [] = []
returnMatrixEight xs = [vetorMax] ++ returnMatrixEight resto
  where
    (vetorMax, resto) = stopinEight xs 0 []
---
transformVector :: [Int] -> String
transformVector [] = []
transformVector (x:xs) = [returnAcid x] ++ transformVector xs

transformInAcid :: [[Int]] -> [String]
transformInAcid [] = []
transformInAcid (x:xs) = [transformVector x] ++ transformInAcid xs



main :: IO ()
main = do

  input <- getLine

  let tree = returnInOrder (read input :: Tree Int) 
  let matrix = returnMatrixEight tree
  let result = transformInAcid matrix
  
  print result
