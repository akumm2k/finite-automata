import Automaton
import DFA 
import NFA 

{-
test nfa
    L = {w | w[-1] == 1 || w[:-2] == 10}
-}
my_q :: [Int]
my_q = [0 .. 4]
my_q0 :: Int
my_q0 = 0 

my_delta :: [ExtMove Int]
my_delta = [
    ExtMove 0 "0" [0],
    ExtMove 0 "" [2],
    ExtMove 0 "1" [0, 1],
    ExtMove 2 "1" [3],
    ExtMove 3 "0" [1]
    ]

my_f :: [Int]
my_f = [1]

-- split an extended move into a non deterministive moves
extMove_to_move :: [ExtMove a] -> [Move a]
extMove_to_move movesN = 
    [Move p c q | (ExtMove p cs q) <- movesN, cs /= "", c <- cs]
    ++ 
    [EMove p q | (ExtMove p "" q) <- movesN]

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 

to_nfa :: DFA a -> NFA a 
to_nfa (DFA q delta q0 f) = NFA q delta q0 f 

{-
TODO: implement subset construction
to_dfa :: NFA a -> DFA [a] 
to_dfa (NFA q delta q0 f) = 
    DFA q' delta' q0' f' 
    where 
-}