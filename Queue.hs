module Queue where

class Queue q where
    empty   :: q a 
    enqueue :: a -> q a -> q a 
    dequeue :: q a -> Maybe (a, q a)
    isEmpty :: q a -> Bool

data FQueue a = FQueue [a] [a]
    deriving (Show)

{-
[a b c] / [f e d]
dequeue at [] / [c b a]
    [a b c] / []
    [b c] / []
-}
instance Queue FQueue where 
    empty = FQueue [] [] 
    isEmpty (FQueue f b) = null f && null b
    enqueue x (FQueue f b) = -- prepend to the back
        FQueue f (x : b)
    dequeue (FQueue [] []) = Nothing
    dequeue (FQueue (x : xs) b) = Just (x, FQueue xs b)
    dequeue (FQueue [] back) = 
        let (x : xs) = reverse back 
        in Just (x, FQueue xs [])

instance Functor FQueue where 
    fmap f (FQueue front back) = 
        FQueue (map f front) (map f back)

instance Foldable FQueue where 
    foldr f a (FQueue front back) = 
        let a1 = foldr f a back 
        in foldr f a1 front