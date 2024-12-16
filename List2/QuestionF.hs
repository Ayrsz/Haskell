data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node x ln rn) = max (1 + alturaArvore ln) (1 + alturaArvore rn)

main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result