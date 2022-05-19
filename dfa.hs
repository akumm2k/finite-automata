module DFA (build_dfa, DFA(..)) where 
    
import Debug.Trace
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

data DFA a = 
    DFA {statesD :: [a], movesD :: [(Move a)], startD :: [a], finalD :: [a]}

instance (Show a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0 ++ " \n" ++
        "F: " ++ show f 

build_dfa :: Ord a => [a] -> [Move a] -> [a] -> [a] -> DFA a
build_dfa q delta q0 f = 
    if is_deterministic (delta, q0)  then DFA q delta q0 f
    else error ("Non-deterministic move detected.")

is_deterministic :: ([Move a], [a]) -> Bool 
is_deterministic (ms, q0) = 
    and [null $ tail q | (Move _ _ q) <- ms] 
    && null [1 | (EMove _ _) <- ms] 
    && null (tail q0)

deltaD :: Eq a => DFA a -> Char -> a -> [a]
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