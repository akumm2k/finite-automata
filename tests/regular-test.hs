import Automaton
import DFA 
import NFA 
import Regular

{-
test nfa
    L = {w | w[-1] == 1 || w[:-2] == 10}
-}
my_q :: [Int]
my_q = [0 .. 4]
my_q0 :: [Int]
my_q0 = [0] 

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

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 

to_nfa :: DFA a -> NFA a 
to_nfa (DFA q delta q0 f) = NFA q delta q0 f 