import Automaton
import NFA

{-
Test NFA:
accepts `[+-]?(([1-9]+\.[1-9]*)|([1-9]*\.[1-9]+))`
-}

my_q :: [Int]
my_q = [0 .. 5]
my_q0 :: [Int]
my_q0 = [0] 

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

my_f :: [Int]
my_f = [5]

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 

n :: NFA Int
n = NFA [0 .. 3] [(EMove 0 [1]), (Move 1 '1' [2]), (EMove 2 [3])] [0] [3]