import Automaton
import DFA 

-- test DFA:
-- L(my_dfa) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
my_q :: [Int]
my_q = [0, 1, 2]

my_delta :: [Move Int]
my_delta = [Move x '0' [x] | x <- my_q] ++ [
    Move 0 '1' [1],
    Move 1 '1' [2],
    Move 2 '1' [0]
    ]

my_q0 :: [Int]
my_q0 = [0] 

my_f :: [Int]
my_f = [0]

my_dfa :: DFA Int
my_dfa = DFA my_q my_delta my_q0 my_f