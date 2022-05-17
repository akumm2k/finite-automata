import Debug.Trace
import Automaton
{-
* DFA: 
(
    Q                           - finite set of states
    delta: Q x Sigma -> P[Q]    - finite set of transitions
    q0                          - starting state
    F                           - set of final states
)
-}

instance Automaton DFA where
    states = statesD 
    start = startD 
    final = finalD 
    delta = deltaD 
    accepts = acceptsD

data DFA a = 
    DFA {statesD :: [a], deltaD :: [(Move a)], startD :: a, finalD :: [a]}

instance (Show a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0 ++ " \n" ++
        "F: " ++ show f 

build_dfa :: Ord a => [a] -> [Move a] -> a -> [a] -> DFA a
build_dfa q delta q0 f = DFA q delta q0 f

delta_star' :: (Eq a, Show a) => String -> a -> DFA a -> Maybe a 
delta_star' [] f _ = Just f 
delta_star' (c : cs) q dfa = 
    let next = [p | (Move q' c' p) <- delta dfa, q == q', c == c']
    in case next of 
        [] -> Nothing 
        [p] -> delta_star' cs p dfa 
        _ -> error ("Non-deterministic move detected " ++ show q 
            ++ " - " ++ [c] ++ " -> " ++ show next)

delta_star :: (Eq a, Show a) => String -> DFA a -> Maybe a 
delta_star s dfa = delta_star' (reverse s) (start dfa) dfa

acceptsD :: (Eq a, Show a) => DFA a -> String -> Bool 
acceptsD dfa s = 
    let mq = delta_star s dfa 
    in case mq of 
        Just q -> q `elem` (final dfa)
        Nothing -> False

-- L(my_dfa) = {w | w \in {0, 1}*, |w|1 % 3 == 0}
my_q :: [Int]
my_q = [0, 1, 2]

my_delta :: [Move Int]
my_delta = [Move x '0' x | x <- my_q] ++ [
    Move 0 '1' 1,
    Move 1 '1' 2,
    Move 2 '1' 0
    ]

my_q0 :: Int
my_q0 = 0 

my_f :: [Int]
my_f = [0]

my_dfa :: DFA Int
my_dfa = DFA my_q my_delta my_q0 my_f