module Automaton where 
import Data.List 

data Move a = 
    Move {from :: a, char :: Char, to :: [a]} 
    | EMove {efrom :: a, eto :: [a]}
    deriving (Eq)

class Automaton at where 
    states :: at a -> [a]
    start :: at a -> [a] 
    final :: at a -> [a]
    moves :: at a -> [(Move a)]
    accepts :: (Eq a, Show a) => at a -> String -> Bool
    delta :: (Eq a, Show a) => at a -> Char -> a -> [a]
    isomorphism :: (Show a, Eq a, Show b, Eq b) => at a -> [b] -> at b

show_states :: Show a => [a] -> String
-- show_states [1, 2, 3] = "1, 2, 3"
show_states qs = intercalate ", " (show <$> qs)

instance (Show a) => Show (Move a) where 
    show (Move q c ps) = 
        "(" ++ show q ++ " - " ++ [c] ++ " -> " ++ show_states ps ++ ")"
    show (EMove q ps) = 
        "(" ++ show q ++ " - " ++ "\\" ++ " -> " ++ show_states ps ++ ")"

rmdups :: Ord a => [a] -> [a]
rmdups = map head . group . sort

alphabet_of :: Automaton at => at a -> [Char]
-- return all the alphabet used in the automaton
alphabet_of at = rmdups [c | (Move _ c _) <- moves at]

differentiate_states :: (Automaton at, Show a, Eq a, Show b, Eq b) 
    => at a -> at b -> (Int, Int, at Int, at Int)
-- map the states to distinct ints
-- return the lengths and new distinct automata
differentiate_states a1 a2 = 
    let (l1, l2) = (ls a1, ls a2) where ls = length . states 
        a1' = isomorphism a1 [1 .. l1]
        a2' = isomorphism a2 [l1 + 1 .. l1 + l2]
    in (l1, l2, a1', a2')