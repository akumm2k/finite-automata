{-
* DFA: 
(
    Q                           - finite set of states
    delta: Q x Sigma -> P[Q]    - finite set of transitions
    q0                          - starting state
    F                           - set of final states
)
-}

data DFA a = 
    DFA [a] [(DMove a)] a [a]

data DMove a =
    DMove a Char a
    deriving Eq

instance (Show a) => Show (DMove a) where 
    show (DMove q c p) = 
        "(" ++ show q ++ " - " ++ [c] ++ " -> " ++ show p ++ ")"

instance (Show a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0 ++ " \n" ++
        "F: " ++ show f 

build_dfa :: Ord a => [a] -> [DMove a] -> a -> [a] -> DFA a
build_dfa q delta q0 f = DFA q delta q0 f

-- L(my_dfa) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
q :: [Int]
q = [0, 1, 2]

delta :: [DMove Int]
delta = [DMove x '0' x | x <- q] ++ [
    DMove 0 '1' 1,
    DMove 1 '1' 2,
    DMove 2 '1' 0
    ]

q0 :: Int
q0 = 0 

f :: [Int]
f = [0]

my_dfa :: DFA Int
my_dfa = DFA q delta q0 f