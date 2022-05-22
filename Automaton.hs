module Automaton where 
import Data.List as List ( group, intercalate, sort )
import Data.Set as Set
    ( Set, empty, fromList, map, null, toList, union, unions )

class Automaton at where 
    states :: at a -> Set a
    start :: at a -> Set a
    final :: at a -> Set a
    moves :: at a -> Set (Move a)
    accepts :: (Ord a, Show a) => at a -> String -> Bool
    delta :: (Ord a, Show a) => at a -> Char -> a -> Set a
    isomorphism :: (Show a, Ord a, Show b, Ord b) => 
        at a -> Set b -> at b

show_states :: Show a => Set a -> String
-- show_states [1, 2, 3] = "1, 2, 3"
show_states qs = intercalate ", " (show <$> (toList qs))

data Move a = 
    Move {from :: a, char :: Char, to :: Set a} 
    | EMove {efrom :: a, eto :: Set a}
    deriving (Eq)

instance (Ord a) => Ord (Move a) where 
    (<=) (Move p _ _) (EMove q _) = p <= q
    (<=) (Move p c1 _) (Move q c2 _) = (p, c1) <= (q, c2)
    (<=) (EMove p _) (EMove q _) = p <= q
    (<=) (EMove _ p) (Move _ _ q) = p <= q
    -- (<=) _ _ = True

instance (Show a) => Show (Move a) where 
    show (Move q c ps) = 
        "(" ++ show q ++ " - " ++ [c] ++ " -> " 
            ++ show_states ps ++ ")"
    show (EMove q ps) = 
        "(" ++ show q ++ " - " ++ "\\" ++ " -> " 
            ++ show_states ps ++ ")"

rmdups :: Ord a => [a] -> [a]
-- this is O(n log n). `nub` uses `Eq a`, so requires O(n)
rmdups = fmap head . group . sort

alphabet_of :: Automaton at => at a -> [Char]
-- return all the alphabet used in the automaton
alphabet_of at = rmdups [c | (Move _ c _) <- toList $ moves at]

differentiate_states :: 
    (Automaton at, Show a, Ord a, Show b, Ord b) 
    => at a -> at b -> (Int, Int, at Int, at Int)
-- map the states to distinct ints
-- return the lengths and new distinct automata
differentiate_states a1 a2 = 
    let (l1, l2) = (ls a1, ls a2) where ls = length . states 
        a1' = isomorphism a1 (fromList [1 .. l1])
        a2' = isomorphism a2 (fromList [l1 + 1 .. l1 + l2])
    in (l1, l2, a1', a2')

set_null :: Set a -> Bool
set_null = Set.null

set_map :: Ord b => (a -> b) -> Set a -> Set b
set_map = Set.map

empty_set :: Set a
empty_set = Set.empty