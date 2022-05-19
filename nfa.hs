module NFA (build_nfa, NFA(..), ExtMove(..), 
    elim_epsilon, extMove_to_move) where 
import Data.List
import Debug.Trace
import Automaton

{-
* NFA: 
(
    finite set of states        q
    finite set of transitions   delta: Q x Sigma -> P(Q)
    starting states             s0
    set of final states         f
)
-}

instance Automaton NFA where
    states = statesN
    start = startN 
    final = finalN 
    moves = movesN
    delta = deltaN
    accepts = acceptsN

data NFA a = 
    NFA {statesN :: [a], movesN :: [(Move a)], startN :: [a], finalN :: [a]}

instance (Show a) =>  Show (NFA a) where 
    show (NFA q delta s0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show s0 ++ " \n" ++
        "F: " ++ show f 

build_nfa :: Ord a => [a] -> [Move a] -> [a] -> [a] -> NFA a
build_nfa q delta s0 f = NFA q delta s0 f

deltaN :: Eq a => NFA a -> Char -> a -> [a]
deltaN nfa c p =
    -- pattern match Move to avoid calling `char` on an epsilon move
    let qs = concat [to m | m@(Move _ _ _) <- movesN nfa, 
            char m == c, from m == p]
    in concatMap (epsilon_closure nfa) qs

{-
q \in (epsilon_closure q)
if p \in (epsilon_closure q) and EMove p r 
    then r \in (epsilon_closure q)
-}
epsilon_closure' :: Eq a => NFA a -> [a] -> [a]
epsilon_closure' n@(NFA q delta s0 f) qs = 
    let new_qs = [r | p <- qs, (EMove p' enp) <- delta, 
            p == p', r <- enp]
    in if null (new_qs \\ qs) then qs 
    else epsilon_closure' n (nub $ qs ++ new_qs)

epsilon_closure :: Eq a => NFA a -> a -> [a]
epsilon_closure n q' = 
    epsilon_closure' n [q']
{-
delta_star(q, wa) | a :: Char, w :: String
    = epsilon_closure (delta(p, a) for all p in delta_star(q, w))
-}
delta_star' :: (Eq a, Show a) => [a] -> NFA a -> String -> Maybe [a] 
delta_star' fs _ [] = Just fs
delta_star' qs nfa (c : cs) = 
    let qs' = nub $ concatMap (epsilon_closure nfa) qs
        next = concatMap (deltaN nfa c) qs'
    in case next of 
        [] -> Nothing 
        _ -> delta_star' next nfa cs

delta_star :: (Eq a, Show a) =>  NFA a -> String -> Maybe [a] 
delta_star nfa = delta_star' (startN nfa) nfa

acceptsN :: (Eq a, Show a) => NFA a -> String -> Bool 
acceptsN nfa s = 
    let reached = delta_star nfa s 
    in case reached of 
        Just qs -> or [f `elem` qs | f <- finalN nfa]
        Nothing -> False

{-
let `p \ q` represent an epsilon transition from p to q
p \ q and p is a starting state, then q is a starting state
p \ q and q is a final state, then p is a final state

for each `p \ q` elimination, create new move `p c r` such that
there is a move `q c r`
-}
elim_epsilon :: Eq a => NFA a -> NFA a 
elim_epsilon n@(NFA q ms q0 f) = 
    let nq0 = concatMap (epsilon_closure n) q0
        nf = [p | q <- f, (EMove p qs) <- ms, q `elem` qs]
        f' = f ++ nf 
        deterministic_moves = [Move p c q' | (Move p c q) <- ms, 
            let q' = concatMap (epsilon_closure n) q]
        new_moves = [Move p c r | (EMove p qs) <- ms, 
            q <- qs, (Move q' c r) <- ms, q == q'
            ]
    in NFA q (deterministic_moves ++ new_moves) nq0 f'


{- 
ExtendedMove NOT by words, but by multiple symbols 
transitioning to the same state

ExtMove 0 "+-" [1, 2] =
    [Move 0 '+' [1, 2], Move '-' [1, 2]]
-}
data ExtMove a = ExtMove a String [a]

-- split an extended move into a non deterministive moves
extMove_to_move :: [ExtMove a] -> [Move a]
extMove_to_move movesN = 
    [Move p c q | (ExtMove p cs q) <- movesN, cs /= "", c <- cs]
    ++ 
    [EMove p q | (ExtMove p "" q) <- movesN]
