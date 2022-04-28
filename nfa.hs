{-
* NFA: 
(
    finite set of states,
    finite set of transitions,
    starting state,
    set of final states
)
-}
data NFA a = 
    NFA [a] [(NMove a)] a [a]

data NMove a =
    NMove a Char [a] |
    Emove a [a]
    deriving Eq 

instance (Show a) => Show (NMove a) where 
    show (NMove q c p) = 
        "(" ++ show q ++ " - '" ++ [c] ++ "' -> " ++ show p ++ ")"
    show (Emove p q) = 
        "(" ++ show q ++ " - " ++ "Eps" ++ " -> " ++ show p ++ ")"

instance (Show a) =>  Show (NFA a) where 
    show (NFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0 ++ " \n" ++
        "F: " ++ show f 

build_nfa :: Ord a => [a] -> [NMove a] -> a -> [a] -> NFA a
build_nfa q delta q0 f = NFA q delta q0 f
