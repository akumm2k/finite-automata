import Automaton
import DFA 
import NFA 
import Regular
import Data.Set 
{-
test nfa
    L = {w | w[-1] == 1 || w[:-2] == 10}
-}
my_q :: Set Int
my_q = fromList [0 .. 4]

my_q0 :: Set Int
my_q0 = singleton 0 

my_delta :: [ExtMove Int]
my_delta = [
    ExtMove 0 "0" [0],
    ExtMove 0 "" [2],
    ExtMove 0 "1" [0, 1],
    ExtMove 2 "1" [3],
    ExtMove 3 "0" [1]
    ]

my_f :: Set Int
my_f = (singleton 1)

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 