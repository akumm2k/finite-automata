import Automaton
import NFA
import Regular
import Data.Set

{- 
ExtendedMove NOT by words, but by multiple symbols 
transitioning to the same state

ExtMove 0 "+-" [1, 2] =
    [Move 0 '+' [1, 2], Move '-' [1, 2]]
-}
data ExtMove a = ExtMove a String [a]

-- split an extended move into a non deterministive moves
extMove_to_move :: Ord a => [ExtMove a] -> Set (Move a)
extMove_to_move movesN = fromList $
    [Move p c (fromList q) | 
        (ExtMove p cs q) <- movesN, cs /= "", c <- cs]
    ++ 
    [EMove p (fromList q) | (ExtMove p "" q) <- movesN]
    

{-
Test NFA:
accepts `[+-]?(([1-9]+\.[1-9]*)|([1-9]*\.[1-9]+))`
-}

my_q :: Set Int
my_q = fromList [0 .. 5]

my_q0 :: Set Int
my_q0 = fromList [0] 

my_delta :: [ExtMove Int]
my_delta = [
    ExtMove 0 "" [1],
    ExtMove 0 "+-" [1],
    ExtMove 1 "." [2],
    ExtMove 1 "0123456789" [1, 4],
    ExtMove 2 "0123456789" [3],
    ExtMove 3 "" [5],
    ExtMove 4 "." [3]
    ]

my_f :: Set Int
my_f = singleton 5

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 

n :: NFA Int
n = NFA (fromList [0 .. 1]) 
    (fromList [(EMove 0 (singleton 1))]) 
    (singleton 0) (singleton 1)