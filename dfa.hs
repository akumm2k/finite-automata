import Debug.Trace
import Automaton
{-
* DFA: 
(
    finite set of states -      Q
    finite set of transitions - delta: Q x Sigma -> P[Q]
    starting state -            q0
    set of final states -       F
)
-}

instance Automaton DFA where
    states = statesD 
    start = startD 
    final = finalD 
    delta = deltaD 
    accepts = acceptsD

data DFA a = 
    DFA {statesD :: [a], movesD :: [(Move a)], startD :: a, finalD :: [a]}

instance (Show a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0 ++ " \n" ++
        "F: " ++ show f 

build_dfa :: Ord a => [a] -> [Move a] -> a -> [a] -> DFA a
build_dfa q delta q0 f = 
    if valid_moves delta then DFA q delta q0 f
    else error ("Non-deterministic move detected.")

valid_moves :: [Move a] -> Bool 
valid_moves moves = 
    and [null $ tail q | (Move _ _ q) <- moves] && 
    null [1 | (EMove _ _) <- moves]

deltaD :: Eq a => DFA a -> Char -> a -> [a]
deltaD dfa c p =
    concat [q | (Move p' c' q) <- movesD dfa, c' == c, p == p']

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
delta_star dfa = delta_star' (start dfa) dfa

acceptsD :: (Eq a, Show a) => DFA a -> String -> Bool 
acceptsD dfa s = 
    let mq = delta_star dfa s 
    in case mq of 
        Just q -> q `elem` (final dfa)
        Nothing -> False

-- test DFA:
-- L(my_dfa) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
my_q :: [Int]
my_q = [0, 1, 2]

my_delta :: [Move Int]
my_delta = [Move x '0' [x] | x <- my_q] ++ [
    Move 0 '1' [1],
    Move 1 '1' [2],
    Move 2 '1' [0]
    ]

my_q0 :: Int
my_q0 = 0 

my_f :: [Int]
my_f = [0]

my_dfa :: DFA Int
my_dfa = DFA my_q my_delta my_q0 my_f