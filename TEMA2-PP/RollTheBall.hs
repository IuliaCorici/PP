{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
import Data.Maybe
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {
                     tip :: Char 
                  ,  pos :: Position
                } 
                deriving (Eq, Ord, Show)
            


-- verPipe | horPipe | topLeft | botLeft | botRight | topRight | emptySpace | emptyCell |
--             startUp | startDown | startLeft | startRight | winUp |winDown | winLeft | winRight 

{-
    Tip de date pentru reprezentarea nivelului curent
-- -}

-- arr::(A.Array (Int, Int) Int)
-- -- arr = A.array ((0, 0), (1, 1)) 
-- --               [((0, 0), 1), ((0, 1), 2),
-- --                ((1, 0), 3), ((1, 1), 4)]


data Level =  Level {
                        matrix :: (A.Array Position Cell) 
                      , dims :: Position
                    } 
                    deriving (Eq, Ord)





{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
-- updateArr :: (A.Array (Int, Int) Cha) -> Int -> (A.Array (Int, Int) Int)
-- updateArr arr  i = arr A.// [((1,i), 5) | i <- [0, 1]]

-- elemAt :: (A.Array (Int, Int) Int) -> (Int, Int) -> Int
-- elemAt arr pos = arr A.! pos

elemsListLine :: Level -> Int -> [Char]
elemsListLine (Level a b) line = (A.elems arr)
    where
        arr = (A.array ((0, 0), (0, (snd b))) [((0, i), (tip (a A.! (line, i)))) | i <- [0 .. snd b]]);

instance Show Level 
    where show (Level a b) = foldl (++) [endl] (map (\x -> (elemsListLine (Level a b) x) ++ [endl]) [0 .. fst b]) 

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel dimensions = (Level (A.array ((0,0), dimensions) [((i, j), (Cell '░' (i, j)))
                        | i <- [0 .. (fst dimensions)], j <- [0 .. (snd dimensions)]]) dimensions)

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (c, posx) (Level a b) = (Level arr1 b)
    where arr1 = if (((snd posx) <= (snd b)) && ((fst posx) <= (fst b)) && ((fst posx) > -1) && ((snd posx) > -1))
                 then if ((tip (a A.! posx)) == '░') then (a A.// [(posx, (Cell c posx))]) else a
                 else a

{-((tip (a A.! posx)) == '░') || 
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel dimx list_c_p = foldl (\x y-> addCell y x) (emptyLevel dimx) list_c_p
    


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell posy direc (Level a b)
    | (((tip (a A.! posy)) == '┴') ||  ((tip (a A.! posy)) == '┬') ||  ((tip (a A.! posy)) == '┤') || ((tip (a A.! posy)) == '├')) = (Level a b)
    | (((tip (a A.! posy)) == '╨') ||  ((tip (a A.! posy)) == '╥') ||  ((tip (a A.! posy)) == '╡') || ((tip (a A.! posy)) == '╞')) = (Level a b)

    | direc == North = if (((snd posy) <= (snd b)) && ((fst posy) -1 <= (fst b)) && ((fst posy) - 1 > -1) && ((snd posy) > -1))
                       then if ((tip (a A.! (((fst posy) - 1), (snd posy)))) == '░') 
                            then (Level (a A.// [((((fst posy) - 1), (snd posy)), (Cell (tip (a A.! posy)) (((fst posy) - 1), (snd posy)))), (posy, (Cell '░' posy))]) b)
                            else (Level a b)
                        else (Level a b)

    | direc == South = if (((snd posy) <= (snd b)) && ((fst posy) +1 <= (fst b)) && ((fst posy) + 1 > -1) && ((snd posy) > -1))
                       then if ((tip (a A.! (((fst posy) + 1), (snd posy)))) == '░') 
                            then (Level (a A.// [((((fst posy) + 1), (snd posy)), (Cell (tip (a A.! posy)) (((fst posy) + 1), (snd posy)))), (posy, (Cell '░' posy))]) b)
                            else (Level a b)
                        else (Level a b)

    | direc == East =  if (((snd posy) + 1 <= (snd b)) && ((fst posy) <= (fst b)) && ((fst posy) > -1) && ((snd posy) + 1 > -1))
                       then if ((tip (a A.! ((fst posy), (snd posy + 1)))) == '░') 
                            then (Level (a A.// [(((fst posy), (snd posy + 1)), (Cell (tip (a A.! posy)) ((fst posy), (snd posy + 1)))), (posy, (Cell '░' posy))]) b)
                            else (Level a b)
                        else (Level a b)

    | direc == West =  if (((snd posy) - 1 <= (snd b)) && ((fst posy) <= (fst b)) && ((fst posy) > -1) && ((snd posy) - 1 > -1))
                       then  if ((tip (a A.! ((fst posy), (snd posy - 1)))) == '░') 
                             then (Level (a A.// [(((fst posy), (snd posy - 1)), (Cell (tip (a A.! posy)) ((fst posy), (snd posy - 1)))), (posy, (Cell '░' posy))]) b)
                             else (Level a b)
                        else (Level a b)
                        
    | otherwise = (Level a b)

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell pipe1 _) (Cell pipe2 _)  dir
    |  pipe1 == botLeft  && (pipe2 == horPipe  || pipe2 == botRight || pipe2 == topRight || pipe2 == winLeft)   && dir ==  East  = True
    |  pipe1 == botLeft  && (pipe2 == verPipe  || pipe2 == topRight || pipe2 == topLeft  || pipe2 == winDown)   && dir ==  North = True

    |  pipe1 == topLeft  && (pipe2 == horPipe  || pipe2 == botRight || pipe2 == topRight || pipe2 == winLeft)   && dir ==  East  = True
    |  pipe1 == topLeft  && (pipe2 == verPipe  || pipe2 == botLeft  || pipe2 == botRight || pipe2 == winUp)     && dir ==  South = True

    |  pipe1 == topRight && (pipe2 == verPipe  || pipe2 == botLeft  || pipe2 == botRight || pipe2 == winUp)     && dir ==  South = True
    |  pipe1 == topRight && (pipe2 == horPipe  || pipe2 == topLeft  || pipe2 == botLeft  || pipe2 == winRight)  && dir ==  West  = True

    |  pipe1 == botRight  && (pipe2 == horPipe || pipe2 == botLeft  || pipe2 == topLeft || pipe2 == winRight) && dir ==  West  = True
    |  pipe1 == botRight  && (pipe2 == verPipe || pipe2 == topRight || pipe2 == topLeft || pipe2 == winDown)  && dir ==  North = True

    |  pipe1 == verPipe  && (pipe2 == verPipe  || pipe2 == botLeft  || pipe2 == botRight || pipe2 == winUp)    && dir ==  South = True
    |  pipe1 == verPipe  && (pipe2 == verPipe  || pipe2 == topRight || pipe2 == topLeft  || pipe2 == winDown)  && dir ==  North = True

    |  pipe1 == horPipe  && (pipe2 == horPipe  || pipe2 == botRight  || pipe2 == topRight || pipe2 == winLeft)  && dir ==  East  = True
    |  pipe1 == horPipe  && (pipe2 == horPipe  || pipe2 == botLeft || pipe2 == topLeft  || pipe2 == winRight) && dir ==  West  = True

    |  pipe1 == startUp    && (pipe2 == verPipe || pipe2 == topLeft  || pipe2 == topRight)  && dir == North = True
    |  pipe1 == startDown  && (pipe2 == verPipe || pipe2 == botLeft  || pipe2 == botRight)  && dir == South = True
    |  pipe1 == startLeft  && (pipe2 == horPipe || pipe2 == botLeft  || pipe2 == topLeft )  && dir == West  = True
    |  pipe1 == startRight && (pipe2 == horPipe || pipe2 == botRight || pipe2 == topRight)  && dir == East  = True

    | otherwise = False

 

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

findStart :: Level -> Cell
findStart (Level a b) =  [(Cell c (i, j)) | i <- [0 .. fst b], j <- [0 .. snd b], c <- startCells, (tip (a A.! (i, j))) == c] !! 0


findWin :: Level -> Cell
findWin (Level a b) =  [(Cell c (i, j)) | i <- [0 .. fst b], j <- [0 .. snd b], c <- winningCells, (tip (a A.! (i, j))) == c] !! 0
                            



canGetThere :: Position -> Level -> Directions -> Maybe (Position, Directions)
canGetThere (x, y) (Level a b) dir_from

    | (((y - 1 <= (snd b)) && (x <= (fst b)) && (x > -1) && (y - 1 > -1)) && ((connection (a A.! (x, y)) (a A.! (x, y - 1)) West) == True) && (dir_from /= West) ) = Just ((x, y - 1), East)

    | (((y + 1 <= (snd b)) && (x <= (fst b)) && (x > -1) && (y + 1 > -1)) && ((connection (a A.! (x, y)) (a A.! (x, y + 1)) East) == True) && (dir_from /= East)) = Just ((x, y + 1), West)

    | (((y <= (snd b)) && (x - 1 <= (fst b)) && (x - 1 > -1) && (y > -1)) && ((connection (a A.! (x, y)) (a A.! (x - 1, y)) North) == True) && (dir_from /= North)) = Just ((x - 1, y), South)

    | (((y <= (snd b)) && (x + 1 <= (fst b)) && (x + 1 > -1) && (y > -1)) && ((connection (a A.! (x, y)) (a A.! (x + 1, y)) South) == True) && (dir_from /= South)) = Just ((x + 1, y), North)

    | otherwise = Nothing


checkNext :: Position -> Level -> Directions -> Bool
checkNext (x, y) (Level a b) old_dir = if (isNothing (canGetThere (x, y) (Level a b) old_dir))
                                       then False
                                       else if ((tip (a A.! next) == (tip (findWin (Level a b)))))
                                            then True
                                            else (checkNext next (Level a b) next_dir)
                                            where (next, next_dir) = fromJust (canGetThere (x, y) (Level a b) old_dir)
    
    
wonLevel :: Level -> Bool
wonLevel (Level a b) 
    | (tip (findStart (Level a b)) == startDown) = checkNext (pos (findStart (Level a b))) (Level a b) North
    | (tip (findStart (Level a b)) == startUp) = checkNext (pos (findStart (Level a b))) (Level a b) South
    | (tip (findStart (Level a b)) == startLeft) = checkNext (pos (findStart (Level a b))) (Level a b) East
    | (tip (findStart (Level a b)) == startRight) = checkNext (pos (findStart (Level a b))) (Level a b) West
   
getAllLevels (Level a b) = [(((i, j), dir), (moveCell (i,j) dir (Level a b))) | i <- [0 .. fst b], j <- [0 .. snd b], dir <- [North, West, East, South]]
instance ProblemState Level (Position, Directions) where

    successors (Level a b) = [(action, (Level x y)) | (action, (Level x y)) <- (getAllLevels (Level a b)), x /= a]
        
    isGoal = wonLevel 
    reverseAction  = undefined

