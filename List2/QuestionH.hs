data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show, Eq)

faces :: Direction -> [Command] -> Direction
faces (dircI) [] = dircI
faces (dircI) (x:xs)
    | x == TurnLeft && dircI == East = faces North xs
    | x == TurnLeft && dircI == North = faces West xs
    | x == TurnLeft && dircI == West = faces South xs
    | x == TurnLeft && dircI == South = faces East xs
    | x == TurnRight && dircI == East = faces South xs
    | x == TurnRight && dircI == South = faces West xs
    | x == TurnRight && dircI == West = faces North xs
    | x == TurnRight && dircI == North = faces East xs
    | otherwise = faces dircI xs


main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result