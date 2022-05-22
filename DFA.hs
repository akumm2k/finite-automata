module DFA where 
    
import Automaton
import Data.Set as Set
    ( difference, empty, fromList, singleton, toList, Set, unions )
import Data.List as List ( null, (\\), nub, sort )
import Data.Array ( listArray, (!) )
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
    accepts = acceptsD
    isomorphism = isomorphismD
    alphabet_of = alphabet_of_dfa

data DFA a = 
    DFA {statesD :: Set a, movesD :: Set (Move a), 
        startD :: Set a, finalD :: Set a}

instance (Show a, Ord a) =>  Show (DFA a) where 
    show (DFA q delta q0 f) = 
        "Q: " ++ show (toList q) ++ " \n" ++
        "delta: " ++ show (sort (toList delta)) ++ " \n" ++ 
        "q0: " ++ show q0' ++ " \n" ++
        "F: " ++ show (toList f) 
        where [q0'] = toList q0 

build_dfa :: Ord a => Set a -> Set (Move a) 
    -> Set a -> Set a -> DFA a
build_dfa q delta q0 f = 
    if is_deterministic (delta, q0) then DFA q delta q0 f
    else error ("Non-deterministic move detected.")

is_deterministic :: (Set (Move a), Set a) -> Bool 
-- return true if the moves have only one sink state
is_deterministic (ms, q0) = 
    and [null $ tail (toList q) | (Move _ _ q) <- toList ms] 
    && null [1 | (EMove _ _) <- toList ms] 
    && (null $ tail $ toList q0)

deltaD :: Ord a => DFA a -> Char -> a -> Set a
-- return the transition from p w/ c in dfa
deltaD dfa c p = unions
    [to m | m <- toList $ movesD dfa, char m == c, from m == p]

{-
delta_star(q, wa) | w :: String, a :: Char 
    = delta( delta_star(q, w), a ) 
-}
delta_star' :: (Ord a, Show a) => a -> DFA a -> String -> Maybe a 
delta_star' f _ [] = Just f 
delta_star' q dfa (c : cs) = 
    let next = delta dfa c q
    in case toList next of 
        [] -> Nothing 
        [p] -> delta_star' p dfa cs
        _ -> error ("Non-deterministic move detected " ++ show q 
            ++ " - " ++ [c] ++ " -> " ++ show next)

delta_star :: (Ord a, Show a) => DFA a -> String -> Maybe a 
delta_star dfa = delta_star' s dfa
    where [s] = toList $ start dfa

acceptsD :: (Ord a, Show a) => DFA a -> String -> Bool 
acceptsD dfa s = 
    let mq = delta_star dfa s 
    in case mq of 
        Just q -> q `elem` (final dfa)
        Nothing -> False

alphabet_of_dfa :: DFA a -> [Char]
alphabet_of_dfa d = rmdups [c | (Move _ c _) <- toList $ movesD d]

isomorphismD :: (Show a, Ord a, Show b, Ord b) => 
    DFA a -> Set b -> DFA b
-- return an isomorphic dfa w/ states(DFA) renamed to qs'
isomorphismD d@(DFA q moves q0 f) qs'' = 
    let qs = toList $ states d
        qs' = toList qs''
        h = zip qs qs'
        [q0'] = toList q0
        moves' = fromList [(Move hp c (singleton hq)) | 
            (Move p c q) <- toList moves, let [q'] = toList q, 
            let Just hp = (lookup p h), let Just hq = lookup q' h]
        Just q0'' = lookup q0' h
        f' = fromList [x' | x <- toList f, let Just x' = lookup x h]
    in DFA (fromList qs') moves' (singleton q0'') f'
-- isomorphismD _ _ = error "non-determinsm deteced"

reverse_dict :: (Ord a, Ord b) => [(a, b)] -> [(b, Set a)] 
-- reverse an assoc list 
-- [(0,[0,0]),(1,[0,0])] -> [([0,0],[0,1])]
reverse_dict d = 
    let new_keys = nub $ snd <$> d
    in [(k, v) | k <- new_keys, 
        let v = fromList [k' | k' <- fst <$> d, 
                let (Just v') = lookup k' d, v' == k]
        ]

{-
record transitions from a given part of the partition
split if necessary

split_part my_dfa [0, 1] [[0, 1], [2]] 2 =
    new_parts = [(0,[0,0]),(1,[0,1])]
    splits = [[0], [1]]
    (new_parts_len, zip [parts_len + 1 ..] splits)
        = (3,[(3,[0]),(4,[1])])
        = (added_len, partitions with new ids)
We compute added_len to compute the length of the partition 
and avoid using `length`
-}
split_part :: (Show a, Ord a) => 
    DFA a -> Set a -> [(Set a)] -> Int -> String -> (Int, [Set a])
split_part dfa part parts parts_len alphabet =
    let new_parts = [(p, ids) | p <- toList part, 
            let ids = [part_id q | c <- alphabet, 
                    let q = delta dfa c p]]
        splits =  snd <$> reverse_dict new_parts
        added_len = length splits - 1
    in (added_len, splits)
    where 
        parts_arr = listArray (0, parts_len - 1) parts
        part_id q = 
            if null q then -1 else
            let q' = head $ toList q in
            head [i | i <- [0 .. (parts_len - 1)], 
                q' `elem` (parts_arr ! i)]

{-
* Table filling algorithm to find equivalent states
at most n iterations where n = number of states in the dfa
at each itr,
    for each part in the partition
        for each state
            record its transition to other partition 
        split the part based on the transitions 
potential optimization: may finish early if the partitions don't change
-}
state_partition :: (Show a, Ord a) => 
    DFA a -> String -> [Set a] -> Int -> Int -> Int -> Int -> (Int, [Set a])
-- return a list of equivalent states using the table filling algorithm
-- described above
state_partition d _ ps ps_len num_states i j 
    | j >= i || ps_len == num_states = (ps_len, ps)

state_partition d alphabet ps ps_len num_states i j = 
    let x = [split_part d part ps ps_len alphabet | part <- ps]
        ps' = concat $ snd <$> x 
        added_len = sum $ fst <$> x
    in (state_partition d alphabet ps' (ps_len + added_len) 
        num_states i (j + 1))

{-
remove unreachable states
perform table filling algorithm to find equivalent states
    first equivalence partition: [[non-final states], [final states]]
    at most `size_of(states)` iterations
        potential optimization: stop when the partitions are not split anymore
update transitions based on the equivalent states
-}
minimize :: (Ord a, Show a) => DFA a -> DFA Int
minimize d = 
    let alphabet = alphabet_of d
        -- remove unreachable states
        reachable = dfs d adjacentD alphabet
        unreachable = states d `difference` reachable
        fin = final d `difference` unreachable
        non_final = reachable `difference` fin
        n = length reachable
        p1 = (fromList [non_final, fin]) `difference` (fromList [empty])
        -- get equivalent states
        (l, ps) = state_partition d alphabet (toList p1) (length p1) n n 0
        -- build minimized dfa
        q = [0 .. l - 1] 
        p_to_id = zip ps q
        delta = update_delta d reachable p_to_id  
        f = fromList [get_id p_to_id f| f <- toList fin]
        q0 = fromList [get_id p_to_id $ head (toList $ start d)]
    in DFA (fromList q) delta q0 f 

update_delta :: Ord a => DFA a -> Set a -> [(Set a, Int)] -> Set (Move Int)
update_delta d@(DFA q' del' q0' f') reachable p_to_id = 
    fromList [Move a' c (singleton b') | (Move a c b'') <- toList $ movesD d,
        let [b] = toList b'',
        a `elem` reachable,
        let a' = get_id p_to_id a, 
        let b' = get_id p_to_id b
    ]

get_id :: Ord a => [(Set a, Int)] -> a -> Int
-- return set ids used in the table filling algorithm
get_id p_to_id x = 
    head [id | (part, id) <- p_to_id, x `elem` part]

adjacentD :: (Show a, Ord a) => DFA a -> String -> a -> [a]
-- return a list of adjacent states of p in DFA d 
adjacentD d alphabet p = 
    [head $ toList qs | c <- alphabet, let qs = delta d c p, qs /= empty] 
