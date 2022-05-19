module DFA (build_dfa, DFA(..)) where 
    
import Debug.Trace
import Prelude
import Automaton
{-
* DFA: 
(
    finite set of states -      Q
    finite set of transitions - delta: Q x Sigma -> Q
    starting state -            q0
    set of final states -       F
)
-}

instance Automaton DFA where
    states = statesD 
    start = startD 
    final = finalD 
    delta = deltaD 
    moves = movesD
    accepts = acceptsD
    isomorphism = isomorphismD

data DFA a = 
    DFA {statesD :: [a], movesD :: [(Move a)], startD :: [a], finalD :: [a]}

instance (Show a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0' ++ " \n" ++
        "F: " ++ show f 
        where [q0'] = q0 

build_dfa :: Ord a => [a] -> [Move a] -> [a] -> [a] -> DFA a
build_dfa q delta q0 f = 
    if is_deterministic (delta, q0)  then DFA q delta q0 f
    else error ("Non-deterministic move detected.")

is_deterministic :: ([Move a], [a]) -> Bool 
-- return true if the moves have only one sink state
is_deterministic (ms, q0) = 
    and [null $ tail q | (Move _ _ q) <- ms] 
    && null [1 | (EMove _ _) <- ms] 
    && null (tail q0)

deltaD :: Eq a => DFA a -> Char -> a -> [a]
-- return the transition from p w/ c in dfa
deltaD dfa c p =
    concat [to m | m <- movesD dfa, char m == c, from m == p]

{-
delta_star(q, wa) | w :: String, a :: Char 
    = delta( delta_star(q, w), a ) 
-}
delta_star' :: (Eq a, Show a) => a -> DFA a -> String -> Maybe a 
delta_star' f _ [] = Just f 
delta_star' q dfa (c : cs) = 
    let next = delta dfa c q
    in case next of 
        [] -> Nothing 
        [p] -> delta_star' p dfa cs
        _ -> error ("Non-deterministic move detected " ++ show q 
            ++ " - " ++ [c] ++ " -> " ++ show next)

delta_star :: (Eq a, Show a) => DFA a -> String -> Maybe a 
delta_star dfa = delta_star' s dfa
    where [s] = start dfa

acceptsD :: (Eq a, Show a) => DFA a -> String -> Bool 
acceptsD dfa s = 
    let mq = delta_star dfa s 
    in case mq of 
        Just q -> q `elem` (final dfa)
        Nothing -> False

isomorphismD :: (Show a, Eq a, Show b, Eq b) => DFA a -> [b] -> DFA b
-- return an isomorphic dfa w/ states(DFA) renamed to qs'
isomorphismD d@(DFA q moves [q0] f) qs' = 
    let qs = states d
        h = zip qs qs'
        moves' = [(Move hp c [hq]) | (Move p c [q]) <- moves, 
            let Just hp = (lookup p h), let Just hq = lookup q h]
        Just q0' = lookup q0 h
        f' = [x' | x <- f, let Just x' = lookup x h]
    in DFA qs' moves' [q0'] f'
isomorphismD _ _ = error "non-determinsm deteced"