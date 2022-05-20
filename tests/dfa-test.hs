import Automaton
import DFA 

-- test DFA:
-- L(d1) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
q1 :: [Int]
q1 = [0, 1, 2]

del1 :: [Move Int]
del1 = [Move x '0' [x] | x <- q1] ++ [
    Move 0 '1' [1],
    Move 1 '1' [2],
    Move 2 '1' [0]
    ]

q01 :: [Int]
q01 = [0] 

f1 :: [Int]
f1 = [0]

d1 :: DFA Int
d1 = DFA q1 del1 q01 f1


q2 :: [Int]
q2 = [0, 1, 2, 3]

del2 :: [Move Int]
del2 = [Move x '0' [x] | x <- q2] ++ [
    Move 0 '1' [1],
    Move 1 '1' [2],
    Move 2 '1' [3],
    Move 3 '1' [0]
    ]

q02 :: [Int]
q02 = [0] 

f2 :: [Int]
f2 = [0, 2]

d2 :: DFA Int
d2 = DFA q2 del2 q02 f2

