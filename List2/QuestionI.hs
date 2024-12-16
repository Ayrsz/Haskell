data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

convert2DNA :: Int -> Char
convert2DNA x 
    | number == 0 = 'E'
    | number == 1 = 'M'
    | number == 2 = 'A'
    | number == 3 = 'C'
    | number == 4 = 'S'
    where
        number = x `mod` 5

preOrder :: Tree t -> [t]
preOrder (Nilt)                     = []
preOrder (Node k (Nilt)   (Nilt))   = [k]
preOrder (Node k (Node n) (Node m)) = [k] ++ preOrder Node n ++ preOrder m
preOrder (Node k (Nilt)   (Node m)) = [k] ++ preOrder Node m
preOrder (Node k (Node n) (Nilt))   = [k] ++ preOrder Node n

main :: IO ()
main = do

  input <- getLine

  let result = dna1 (read input :: Tree Int)

  print result