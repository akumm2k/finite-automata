import Automaton
import DFA
import Regular 
import Data.Set 

-- test DFA:
-- L(d1) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
q1 :: Set Int
q1 = fromList [0, 1, 2]

del1 :: Set (Move Int)
del1 = fromList
    ([Move x '0'  (singleton x) | x <- toList q1] ++ [
    Move 0 '1' (singleton 1),
    Move 1 '1' (singleton 2),
    Move 2 '1' (singleton 0)
    ])

q01 :: Set Int
q01 = singleton 0 

f1 :: Set Int
f1 = singleton 0

d1 :: DFA Int
d1 = DFA q1 del1 q01 f1


q2 :: Set Int
q2 = fromList [0, 1, 2, 3]

del2 :: Set (Move Int)
del2 = fromList  
    ([Move x '0' (fromList [x]) | x <- toList q2] ++ [
    Move 0 '1' (fromList [1]),
    Move 1 '1' (fromList [2]),
    Move 2 '1' (fromList [3]),
    Move 3 '1' (fromList [0])
    ])

q02 :: Set Int
q02 = fromList [0] 

f2 :: Set Int
f2 = fromList [0, 2]

d2 :: DFA Int
d2 = DFA q2 del2 q02 f2


q3 = fromList [0, 1, 2]
del3 = fromList [
    Move 0 'a' (singleton 1),
    Move 0 'b' (singleton 2),
    Move 1 'b' (singleton 1),
    Move 2 'a' (singleton 2)
    ]
q03 = (singleton 0)

f3 = fromList [1, 2]