{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ProblemState where

{-
    Expune funcțiile necesare construirii spațiului stărilor, pentru o problemă
    oarecare.

    `s` și `a` reprezintă tipurile stărilor, respectiv acțiunilor
    care transformă o stare în alta.

    Sintaxa `s -> a` din antetul clasei semnifică faptul că `s` îl determină
    în mod unic pe `a`.
-}

class ProblemState s a | s -> a where
    -- getAllLevels :: s -> [(a, s)]
    {-
        Pentru starea curentă, furnizează lista perechilor
        (acțiune, stare următoare).
    -}

    successors :: s -> [(a, s)]

    {-
        Întoarce `True` dacă starea curentă este finală.
    -}

    isGoal :: s -> Bool
    {-
        Primește un tuplu (a1, s1) pentru a reveni
        la starea precedentă stării s1, adică s0,
        prin inversarea acțiunii a1 cu a0.
        Valoarea de retur este (a0,s0)
        Exemplu: ((South, (1, 0)),s1) -> ((North, (2, 0)),s0)
    -}

    reverseAction :: (a, s) -> (a, s)

