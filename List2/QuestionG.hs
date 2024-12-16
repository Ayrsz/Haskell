data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt val) = val
evalTree (Node SUM ln rn) = evalTree ln + evalTree rn
evalTree (Node MUL ln rn) = evalTree ln * evalTree rn
evalTree (Node SUB ln rn) = evalTree ln - evalTree rn

main = do
    s <- getLine
    let result = evalTree (read s)
    print result