type Comando = String
type Valor = Int

getFirst :: (a, b) -> a
getFirst (x, y) = x

getSecond :: (a, b) -> b
getSecond (x, y) = y


executa :: [(Comando, Valor)] -> Int -> Int  
executa [] result = result 
executa (x:xs) result
    | c == "Multiplica" = executa xs (result*v)
    | c == "Soma" =  executa xs (result+v)
    | c == "Subtrai"=  executa xs (result-v)
    | (c == "Divide" && v == 0) = -666 
    | c == "Divide" =  executa xs (div result v)
    where
        c = getFirst x
        v = getSecond x

main = do
    a <- getLine
    let result = executa (read a) 0
    print result