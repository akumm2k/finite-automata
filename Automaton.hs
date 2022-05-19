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
    delta :: (Eq a) => at a -> Char -> a -> [a]

instance (Show a) => Show (Move a) where 
    show (Move q c p) = 
        "(" ++ show q ++ " - " ++ [c] ++ " -> " ++ show p ++ ")"
    show (EMove q ps) = 
        "(" ++ show q ++ " - " ++ "\\" ++ " -> " ++ show ps ++ ")"

rmdups :: Ord a => [a] -> [a]
rmdups = map head . group . sort

alphabet_of :: Automaton at => at a -> [Char]
alphabet_of at = rmdups [c | (Move _ c _) <- moves at]