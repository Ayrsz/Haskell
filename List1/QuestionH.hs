
isReplica :: String -> Int -> Char -> Bool
isReplica [] _ _= False
isReplica (s:str) n letra
  | achaSubsequencia (s:str) letra >= n = True
  | otherwise = isReplica str n letra
  


achaSubsequencia :: String -> Char -> Int
achaSubsequencia [] _ = 0
achaSubsequencia (s:str) letra
  | s == letra = 1 + achaSubsequencia str letra
  | otherwise = 0


main = do
    a <- getLine 
    b <- getLine 
    c <- getLine 
    let result = isReplica a (read b) (head c)
    print result