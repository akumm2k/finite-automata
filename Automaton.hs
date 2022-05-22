module Automaton where 
import Data.List as List ( group, intercalate, sort, (\\) )
import Data.Set as Set
    ( Set, empty, fromList, map, null, toList, union, unions )

class Automaton at where 
    states :: at a -> Set a
    start :: at a -> Set a
    final :: at a -> Set a
    accepts :: (Ord a, Show a) => at a -> String -> Bool
    delta :: (Ord a, Show a) => at a -> Char -> a -> Set a
    isomorphism :: (Show a, Ord a, Show b, Ord b) => 
        at a -> Set b -> at b
    alphabet_of :: at a -> [Char]

show_states :: Show a => Set a -> String
-- show_states [1, 2, 3] = "1, 2, 3"
show_states qs = intercalate ", " (show <$> (toList qs))

rmdups :: Ord a => [a] -> [a]
-- this is O(n log n). `nub` uses `Eq a`, so requires O(n ^ 2)
rmdups = fmap head . group . sort

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

dfs :: (Show a, Ord a, Automaton at) => 
    at a -> (at a -> String -> a -> [a]) -> String -> Set a
-- find all reachable states in DFA d
dfs d adjacent alphabet = fromList $ reverse $ loop s s
    where 
        loop visited [] = visited 
        loop visited (v : vs) = 
            let ns = adjacent d alphabet v \\ visited 
            in loop (ns ++ visited) (ns ++ vs)
        s = toList $ start d