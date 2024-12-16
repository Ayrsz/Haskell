contagemNotas :: String -> [String] -> Int
contagemNotas _ [] = 0
contagemNotas (n:ota) ((letra:sinal):str)
    | n < letra = 1 + contagemNotas (n:ota) (str) -- A- > B
    | n == letra && ota == sinal = 1 + contagemNotas (n:ota) (str) 
    | n == letra && (ota == "") && sinal == "-" = 1 + contagemNotas (n:ota) (str) 
    | n == letra && (ota == "+") && (sinal == "-" || sinal == "") = 1 + contagemNotas (n:ota) (str) 
    | otherwise = contagemNotas (n:ota) (str) 

cvtSistemaAmericano :: [String] -> [Float]
cvtSistemaAmericano [] = []
cvtSistemaAmericano (s:str) = [x] ++ cvtSistemaAmericano str
    where 
        x = tabelaVal s

calculaMedia :: [Float] -> Float
calculaMedia xs = (foldr (+) 0 xs)/fromIntegral((length xs))

tabelaVal :: String -> Float
tabelaVal s
     | s == "A+"= 9.75
     | s == "A"  = 9.3  
     | s == "A-" = 9.0 
     | s == "B+" = 8.75 
     | s == "B"  = 8.3
     | s == "B-" = 8.0 
     | s == "C+" = 7.75   
     | s == "C"  = 7.3 
     | s == "C-" = 7.0 
     | s == "D+" = 6.75     
     | s == "D"  = 6.3      
     | s == "D-" = 6.0      
     | s == "F"  = 5.9

invtVal :: Float -> String
invtVal x
    | x >=  9.75 = "A+"
    | x >=  9.3 = "A"  
    | x >=  9.0 = "A-" 
    | x >=  8.7 = "B+" 
    | x >=  8.3 = "B" 
    | x >=  8.0 = "B-" 
    | x >=  7.7 = "C+" 
    | x >=  7.3 = "C"  
    | x >=  7.0 = "C-"
    | x >=  6.7 = "D+"    
    | x >=  6.3 = "D"     
    | x >=  6.0 = "D-"    
    | otherwise = "F" 

main = do
    a <- getLine
    b <- getLine
    let notas = cvtSistemaAmericano (read a)
    let media = calculaMedia notas
    let nota = invtVal media
    print (contagemNotas nota (read b))