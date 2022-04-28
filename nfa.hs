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

lambda_closure :: Eq a => a -> NFA a -> [a]
lambda_closure q' (NFA q delta q0 f) = 
    q' : concat [r | (Emove p r) <- delta, p == q']

data ExtMove a = ExtMove a String [a]

q :: [Int]
q = [0 .. 5]
delta :: [ExtMove Int]
delta = [
    ExtMove 0 "" [1],
    ExtMove 0 "+-" [1],
    ExtMove 1 "." [2],
    ExtMove 1 "0123456789" [1, 4],
    ExtMove 2 "0123456789" [3],
    ExtMove 3 "" [5],
    ExtMove 4 "." [3]
    ]
q0 :: Int
q0 = 0 

f :: [Int]
f = [5]

extMove_to_move :: [ExtMove a] -> [NMove a]
extMove_to_move moves = 
    [NMove p c q | (ExtMove p cs q) <- moves, cs /= "", c <- cs]
    ++ 
    [Emove p q | (ExtMove p "" q) <- moves]

my_nfa :: NFA Int
my_nfa = NFA q (extMove_to_move delta) q0 f 