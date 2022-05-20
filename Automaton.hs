module Automaton where 
import Data.List as List hiding (union)
import Data.Set as Set

data Move a = 
    Move {from :: a, char :: Char, to :: Set a} 
    | EMove {efrom :: a, eto :: Set a}
    deriving (Eq, Ord)

class Automaton at where 
    states :: at a -> Set a
    start :: at a -> Set a
    final :: at a -> Set a
    moves :: at a -> Set (Move a)
    accepts :: (Ord a, Show a) => at a -> String -> Bool
    delta :: (Ord a, Show a) => at a -> Char -> a -> Set a
    isomorphism :: (Show a, Ord a, Show b, Ord b) => at a -> Set b -> at b

show_states :: Show a => Set a -> String
-- show_states [1, 2, 3] = "1, 2, 3"
show_states qs = intercalate ", " (show <$> (toList qs))

instance (Show a) => Show (Move a) where 
    show (Move q c ps) = 
        "(" ++ show q ++ " - " ++ [c] ++ " -> " ++ show_states ps ++ ")"
    show (EMove q ps) = 
        "(" ++ show q ++ " - " ++ "\\" ++ " -> " ++ show_states ps ++ ")"

rmdups :: Ord a => [a] -> [a]
rmdups = fmap head . group . sort

alphabet_of :: Automaton at => at a -> [Char]
-- return all the alphabet used in the automaton
alphabet_of at = rmdups [c | (Move _ c _) <- toList $ moves at]

differentiate_states :: (Automaton at, Show a, Ord a, Show b, Ord b) 
    => at a -> at b -> (Int, Int, at Int, at Int)
-- map the states to distinct ints
-- return the lengths and new distinct automata
differentiate_states a1 a2 = 
    let (l1, l2) = (ls a1, ls a2) where ls = length . states 
        a1' = isomorphism a1 (fromList [1 .. l1])
        a2' = isomorphism a2 (fromList [l1 + 1 .. l1 + l2])
    in (l1, l2, a1', a2')

setCat :: Ord a => Set (Set a) -> Set a 
setCat = Set.foldr union Set.empty 

listSetCat :: Ord a => [Set a] -> [a]
listSetCat [] = [] 
listSetCat [a, b] = toList (a `union` b)
listSetCat (a : bs) = toList a ++ listSetCat bs