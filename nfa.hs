import Data.List
import Debug.Trace
import Automaton

{-
* NFA: 
(
    finite set of states,
    finite set of transitions,
    starting state,
    set of final states
)
-}

instance Automaton NFA where
    states = statesN
    start = startN 
    final = finalN 
    -- delta = deltaN
    accepts = acceptsN

data NFA a = 
    NFA {statesN :: [a], moves :: [(Move a)], startN :: a, finalN :: [a]}

-- data Move a =
--     Move a Char [a] |
--     EMove a [a]
--     deriving Eq 

-- instance (Show a) => Show (Move a) where 
--     show (Move q c p) = 
--         "(" ++ show q ++ " - '" ++ [c] ++ "' -> " ++ show p ++ ")"
--     show (EMove p q) = 
--         "(" ++ show q ++ " - " ++ "Eps" ++ " -> " ++ show p ++ ")"

instance (Show a) =>  Show (NFA a) where 
    show (NFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0 ++ " \n" ++
        "F: " ++ show f 

build_nfa :: Ord a => [a] -> [Move a] -> a -> [a] -> NFA a
build_nfa q delta q0 f = NFA q delta q0 f

deltaN :: Eq a => NFA a -> Char -> a -> [a]
deltaN nfa c p = nub $
    let qs = concat [q | (Move p' c' q) <- moves nfa, c' == c, p == p']
    in concatMap (lambda_closure nfa) qs

{-
q \in (lambda_closure q)
for all r s.t. EMove q r, r \in (lambda_closure q)
-}
lambda_closure :: Eq a => NFA a -> a -> [a]
lambda_closure (NFA q delta q0 f) q' = 
    q' : concat [r | (EMove p r) <- delta, p == q']

{-
delta_star(q, wa) | a :: Char, w :: String
    = lambda_closure (delta(p, a) for all p in delta_star(q, w))
-}
delta_star' :: (Eq a, Show a) => [a] -> NFA a -> String -> Maybe [a] 
delta_star' fs _ [] = Just fs
delta_star' qs nfa (c : cs) = 
    let qs' = nub $ concatMap (lambda_closure nfa) qs
        next = concatMap (deltaN nfa c) qs'
    in 
        case next of 
        [] -> Nothing 
        _ -> delta_star' next nfa cs

delta_star :: (Eq a, Show a) =>  NFA a -> String -> Maybe [a] 
delta_star nfa = delta_star' [startN nfa] nfa

acceptsN :: (Eq a, Show a) => NFA a -> String -> Bool 
acceptsN nfa s = 
    let reached = delta_star nfa s 
    in case reached of 
        Just qs -> or [f `elem` qs | f <- finalN nfa]
        Nothing -> False

{-
Test NFA:
accepts `[+-]?[1-9]*\.[1-9]*`
-}

data ExtMove a = ExtMove a String [a]
{- 
ExtendedMove NOT by words, but by multiple symbols 
transitioning to the same state
-}

my_q :: [Int]
my_q = [0 .. 5]
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
my_q0 :: Int
my_q0 = 0 

my_f :: [Int]
my_f = [5]

-- split an extended move into a non deterministive moves
extMove_to_move :: [ExtMove a] -> [Move a]
extMove_to_move moves = 
    [Move p c q | (ExtMove p cs q) <- moves, cs /= "", c <- cs]
    ++ 
    [EMove p q | (ExtMove p "" q) <- moves]

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 