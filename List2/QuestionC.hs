data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node x ln rn) = max (1 + alturaArvore ln) (1 + alturaArvore rn)



maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node arvore ln rn)  = max (max (maiorDiametro rn) (maiorDiametro ln)) diametro 
    where
        diametro = alturaArvore ln + alturaArvore rn + 1


main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result