data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

maiorDiametro :: (Num t, Ord t) => Tree t -> t
maiorDiametro (Nilt) = 0
maiorDiametro (Node val Nilt Nilt) = 0
maiorDiametro (Node val lt rt) = maxnode - minnode
    where
        maxnode = maxValue (Node val lt rt)
        minnode = minValue (Node val lt rt)

maxValue :: (Num t, Ord t) => Tree t -> t
maxValue (Node val Nilt Nilt) = val
maxValue (Node val lt Nilt) = maximum [val, maxValue lt]
maxValue (Node val Nilt rt) = maximum [val, maxValue rt]
maxValue (Node val lt rt) = maximum [val,  maxValue (lt), (maxValue rt)]

minValue :: (Num t, Ord t) => Tree t -> t
minValue (Node val Nilt Nilt) = val
minValue (Node val lt Nilt) = minimum [val, minValue lt]
minValue (Node val Nilt rt) = minimum [val, minValue rt]
minValue (Node val lt rt) = minimum [val , minValue (lt), (minValue rt)]


main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result