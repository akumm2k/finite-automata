import Automaton
import DFA
import Regular 
import Data.Set 

-- test DFA:
-- L(d1) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
q1 :: Set Int
q1 = fromList [0, 1, 2]

del1 :: Set (DMove Int)
del1 = fromList
    ([DMove x '0'  x | x <- toList q1] ++ [
    DMove 0 '1' 1,
    DMove 1 '1' 2,
    DMove 2 '1' 0
    ])

q01 :: Set Int
q01 = singleton 0 

f1 :: Set Int
f1 = singleton 0

d1 :: DFA Int
d1 = DFA q1 del1 q01 f1


q2 :: Set Int
q2 = fromList [0, 1, 2, 3]

del2 :: Set (DMove Int)
del2 = fromList  
    ([DMove x '0' x | x <- toList q2] ++ [
    DMove 0 '1'  1,
    DMove 1 '1'  2,
    DMove 2 '1'  3,
    DMove 3 '1'  0
    ])

q02 :: Set Int
q02 = fromList [0] 

f2 :: Set Int
f2 = fromList [0, 2]

d2 :: DFA Int
d2 = DFA q2 del2 q02 f2


q3 = fromList [0, 1, 2]
del3 = fromList [
    DMove 0 'a' 1,
    DMove 0 'b' 2,
    DMove 1 'b' 1,
    DMove 2 'a' 2
    ]
q03 = 0

f3 = fromList [1, 2]

-- minimization bug for (abb*|baa*)
q4 = fromList [0, 1, 2, 3, 4]
del4 = fromList [
    DMove 0 'a' 1,
    DMove 0 'b' 2,
    DMove 1 'b' 3,
    DMove 2 'a' 4,
    DMove 4 'a' 4,
    DMove 3 'b' 3
    ]
q04 = 0

f4 = fromList [3, 4]