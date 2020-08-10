{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

-- import RollTheBall
import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Nod {
                    state :: s,
                    action :: Maybe a,
                    parent :: Maybe (Node s a),
                    depth :: Int,
                    children :: [Node s a]

                } deriving (Eq, Show)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}


nodeState :: Node s a -> s
nodeState (Nod s _ _ _ _) = s

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Nod _ _ p _ _) = p

nodeDepth :: Node s a -> Int
nodeDepth (Nod _ _ _ d _) = d

nodeAction :: Node s a -> Maybe a
nodeAction (Nod _ a _ _ _) = a

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Nod _ _ _ _ l) = l

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
createChildren ::  (ProblemState s a, Eq s) => s -> Maybe (Node s a) -> Int -> [Node s a]
createChildren sta root dpt = (foldl (\x  (move, stat) -> x ++ [(nextNode move stat)]) [] (successors sta))
    --  [ (nextNode move stat) |(move, stat) <-(successors sta)]
                            where
                                nextNode m st = (Nod st (Just m) root dpt (childList m st))
                                childList m st = (createChildren st (Just (nextNode m st)) (dpt + 1))
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = (Nod s Nothing Nothing 0 (createChildren s (Just (createStateSpace s)) 1))

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

belongsTo :: Eq s => Node s a -> [Node s a] -> Bool
belongsTo (Nod s m root dpt childList) list = foldl (\ acc x -> ((state x) == s) || acc) False list



bfs :: Ord s => Node s a -> [([(Node s a)], [(Node s a)])]
bfs (Nod s m root dpt childList) =  doBfs  (Nod s m root dpt childList)  [(Nod s m root dpt childList)] []  [(Nod s m root dpt childList)]
    where
    doBfs  nod@(Nod s m root dpt childList)  queue visited disc_at_level
                                    | (null queue) && (null disc_at_level) = [(disc_at_level, queue)]
                                    | otherwise = (disc_at_level, queue) : (doBfs nod_new queue_new visited_new disc_at_level_new)
                                    where 
                                        filteredChildren = filter (\x ->  not (belongsTo x visited)) childList
                                        visited_new = visited ++ [nod] ++ filteredChildren
                                        queue_new = (tail queue) ++ filteredChildren
                                        nod_new = (head queue_new)
                                        disc_at_level_new = filteredChildren



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
