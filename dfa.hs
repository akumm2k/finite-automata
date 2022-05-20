module DFA where 
    
import Debug.Trace
import Prelude
import Automaton
import Data.List
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
    isomorphism = isomorphismD

data DFA a = 
    DFA {statesD :: [a], movesD :: [(Move a)], 
        startD :: [a], finalD :: [a]}

instance (Show a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show q ++ " \n" ++
        "delta: " ++ show delta ++ " \n" ++ 
        "q0: " ++ show q0' ++ " \n" ++
        "F: " ++ show f 
        where [q0'] = q0 

build_dfa :: Ord a => [a] -> [Move a] -> [a] -> [a] -> DFA a
build_dfa q delta q0 f = 
    if is_deterministic (delta, q0)  then DFA q delta q0 f
    else error ("Non-deterministic move detected.")

is_deterministic :: ([Move a], [a]) -> Bool 
-- return true if the moves have only one sink state
is_deterministic (ms, q0) = 
    and [null $ tail q | (Move _ _ q) <- ms] 
    && null [1 | (EMove _ _) <- ms] 
    && null (tail q0)

deltaD :: Eq a => DFA a -> Char -> a -> [a]
-- return the transition from p w/ c in dfa
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

isomorphismD :: (Show a, Eq a, Show b, Eq b) => 
    DFA a -> [b] -> DFA b
-- return an isomorphic dfa w/ states(DFA) renamed to qs'
isomorphismD d@(DFA q moves [q0] f) qs' = 
    let qs = states d
        h = zip qs qs'
        moves' = [(Move hp c [hq]) | (Move p c [q]) <- moves, 
            let Just hp = (lookup p h), let Just hq = lookup q h]
        Just q0' = lookup q0 h
        f' = [x' | x <- f, let Just x' = lookup x h]
    in DFA qs' moves' [q0'] f'
isomorphismD _ _ = error "non-determinsm deteced"

reverse_dict :: (Eq a, Eq b) => [(a, b)] -> [(b, [a])] 
-- reverse an assoc list 
-- [(0,[0,0]),(1,[0,0])] -> [([0,0],[0,1])]
reverse_dict d = 
    let new_keys = nub $ snd <$> d
    in [(k, v) | k <- new_keys, 
        let v = [k' | k' <- fst <$> d, let (Just v') = lookup k' d, 
                v' == k]
        ]

{-
record transitions from a given part of the partition
split if necessary

split_part my_dfa [0, 1] [[0, 1], [2]] 2 =
    new_parts = [(0,[0,0]),(1,[0,1])]
    splits = [[0], [1]]
    (new_parts_len, zip [parts_len + 1 ..] splits)
        = (3,[(3,[0]),(4,[1])])
        = (new_len, partitions with new ids)
-}
split_part :: (Show a, Eq a) => 
    DFA a -> [a] -> [[a]] -> Int -> (Int, [[a]])
split_part dfa part parts parts_len =
    let alphabet = alphabet_of dfa
        new_parts = [(p, ids) | p <- part, 
            let ids = [part_id q | c <- alphabet, 
                    let q = head $ delta dfa c p]]
        splits = snd <$> reverse_dict new_parts
        added_len = length splits - 1
    in (added_len, splits)
    where 
        part_id q = 
            head [i | i <- [0 .. (parts_len - 1)], 
                q `elem` (parts !! i)]

{-
at most n iterations where n = number of states in the dfa
at each itr,
    for each part in the partition
        for each state
            record its transition to other partition 
        split the part based on the transitions 
finish early if the 
-}
state_partition :: (Show a, Eq a) => 
    DFA a -> [[a]] -> Int -> Int -> Int -> Int -> (Int, [[a]])
state_partition d ps ps_len num_states i j 
    | j >= i || ps_len == num_states = (ps_len, ps)
state_partition d ps ps_len num_states i j = 
    let x = [split_part d part ps ps_len | part <- ps]
        ps' = concat $ snd <$> x 
        added_len = sum $ fst <$> x
    in state_partition d ps' (ps_len + added_len) num_states i (j + 1)


minimize :: (Eq a, Show a) => DFA a -> DFA Int
minimize d = 
    let non_final = states d \\ final d 
        n = length $ states d
        (l, ps) = state_partition d [non_final, final d] 2 n n 0
        q = [0 .. l - 1] 
        p_to_id = zip ps q
        delta = update_delta d p_to_id  
        f = nub [get_id p_to_id f| f <- final d]
        q0 = [get_id p_to_id $ head (start d)]
    in DFA q delta q0 f 

update_final :: Eq a => DFA a -> [([a], Int)] -> [Int]
update_final d p_to_id = [get_id p_to_id f| f <- final d]

update_delta :: Eq a => DFA a -> [([a], Int)] -> [Move Int]
update_delta d@(DFA q' del' q0' f') p_to_id = 
    nub [Move a' c [b'] | (Move a c [b]) <- moves d,
                    let a' = get_id p_to_id a, 
                    let b' = get_id p_to_id b
        ]

get_id :: Eq a => [([a], Int)] -> a -> Int
get_id p_to_id x = 
    head [id | (part, id) <- p_to_id, x `elem` part]
        