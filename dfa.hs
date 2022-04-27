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
    DFA a [(DMove a)] a [a]

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

build_nfa :: Ord a => a -> [DMove a] -> a -> [a] -> DFA a
build_nfa q delta q0 f = DFA q delta q0 f