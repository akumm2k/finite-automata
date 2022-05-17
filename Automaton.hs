module Automaton where 

data Move a = 
    Move {from :: a, char :: Char, to :: a} 
    | EMove {efrom :: a, eto :: [a]}
    deriving (Eq)

class Automaton at where 
    states :: at a -> [a]
    start :: at a -> a 
    final :: at a -> [a]
    accepts :: (Eq a, Show a) => at a -> String -> Bool
    delta :: at a -> [(Move a)]

instance (Show a) => Show (Move a) where 
    show (Move q c p) = 
        "(" ++ show q ++ " - " ++ [c] ++ " -> " ++ show p ++ ")"
    show (EMove q ps) = 
        "(" ++ show q ++ " - " ++ "\\" ++ " -> " ++ show ps ++ ")"