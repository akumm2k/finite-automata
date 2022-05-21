module NFA where 
    
import Automaton
import Data.Set as Set
    ( difference, fromList, member, singleton, toList, union, Set )   
import Data.List ( sort )
{-
* NFA: 
(
    finite set of states        q
    finite set of transitions   delta: Q x Sigma -> P(Q)
    starting states             s0
    set of final states         f
)
-}

instance Automaton NFA where
    states = statesN
    start = startN 
    final = finalN 
    moves = movesN
    delta = deltaN
    accepts = acceptsN
    isomorphism = isomorphismN

data NFA a = 
    NFA {statesN :: Set a, movesN :: Set (Move a), 
        startN :: Set a, finalN :: Set a}

instance (Show a, Ord a) =>  Show (NFA a) where 
    show (NFA q delta s0 f) = 
        "Q: " ++ show (toList q) ++ " \n" ++
        "delta: " ++ show (sort $ (toList delta)) ++ " \n" ++ 
        "q0: " ++ show (toList s0) ++ " \n" ++
        "F: " ++ show (toList f) 

build_nfa :: Ord a => 
    Set a -> Set (Move a) -> Set a -> Set a -> NFA a
build_nfa q delta s0 f = NFA q delta s0 f

deltaN :: (Show a, Ord a) => NFA a -> Char -> a -> Set a
-- return the transition from p w/ c in nfa
deltaN nfa c p =
    -- pattern match Move to avoid calling `char` on an
    -- epsilon move. char :: (Move a Char a) -> Char
    let qs = listSetCat [s | m@(Move _ _ s) <- toList $ movesN nfa, 
            char m == c, from m == p]
    in fromList $ listSetCat [epsilon_closure nfa q | q <- qs]

{-
q \in (epsilon_closure q)
if p \in (epsilon_closure q) and EMove p r 
    then r \in (epsilon_closure q)
-}
epsilon_closure' :: (Show a, Ord a) => NFA a -> Set a -> Set a
epsilon_closure' n@(NFA q del s0 f) qs = 
    let new_qs = fromList [r | p <- toList qs, 
            (EMove p' enp) <- toList del, p == p', r <- toList enp]
    in if set_null (new_qs `difference` qs) then qs 
    else epsilon_closure' n (qs `union` new_qs)

epsilon_closure :: (Show a, Ord a) => NFA a -> a -> Set a
epsilon_closure n q' = 
    epsilon_closure' n (singleton q')

{-
delta_star(q, wa) | a :: Char, w :: String
    = epsilon_closure (delta(p, a) for all p in delta_star(q, w))
-}
delta_star' :: (Ord a, Show a) => Set a -> NFA a -> String -> Maybe (Set a)
delta_star' fs n [] = Just (setCat (set_map (epsilon_closure n) fs))
delta_star' qs nfa (c : cs) = 
    let qs' = setCat (set_map (epsilon_closure nfa) qs)
        next = setCat (set_map (deltaN nfa c) qs')
    in if set_null next then Nothing else 
        delta_star' next nfa cs

delta_star :: (Ord a, Show a) =>  NFA a -> String -> Maybe (Set a)
delta_star nfa = delta_star' (startN nfa) nfa

acceptsN :: (Ord a, Show a) => NFA a -> String -> Bool 
acceptsN nfa s = 
    let reached = delta_star nfa s 
    in case reached of 
        Just qs -> or [f `elem` qs | f <- toList $ finalN nfa]
        Nothing -> False

{-
let `p \ q` represent an epsilon transition from p to q
p \ q and p is a starting state, then q is a starting state
p \ q and q is a final state, then p is a final state

for each `p \ q` elimination, create new move `p c r` such that
there is a move `q c r`
-}
elim_epsilon :: (Show a, Ord a) => NFA a -> NFA a 
elim_epsilon n@(NFA q ms q0 f) = 
    let nq0 =  setCat (set_map (epsilon_closure n) q0)
        nf = [p | q <- toList f, (EMove p qs) <- toList ms, 
            q `member` qs]
        f' = f `union` (fromList nf )

        deterministic_moves = [Move p c q' | 
            (Move p c q) <- toList ms, 
            let q' = setCat (set_map (epsilon_closure n) q)]

        new_moves = [Move p c r | (EMove p qs) <- toList ms, 
            q <- toList qs, (Move q' c r) <- toList ms, q == q'
            ]
    in NFA q (fromList $ deterministic_moves ++ new_moves) nq0 f'

isomorphismN :: (Show a, Ord a, Show b, Ord b) => 
    NFA a -> Set b -> NFA b 
-- return an isomorphic NFA with states(NFA) renamed to qs'
isomorphismN n@(NFA q moves s0 f) qs' =
    let qs = states n 
        h = zip (toList qs) (toList qs')
        h_each = (\x -> case lookup x h of 
            Just x' -> x' 
            Nothing -> error ""
            )
        moves' = [(Move hp c hq) | (Move p c q) <- toList moves, 
            let Just hp = lookup p h, let hq = (set_map h_each q)]
            ++ 
            [(EMove hp hq) | (EMove p q) <- toList moves, 
            let Just hp = lookup p h, let hq = (set_map h_each q)]
        f' = set_map h_each f 
        s0' = set_map h_each s0 
    in NFA qs' (fromList moves') s0' f'
    
{- 
ExtendedMove NOT by words, but by multiple symbols 
transitioning to the same state

ExtMove 0 "+-" [1, 2] =
    [Move 0 '+' [1, 2], Move '-' [1, 2]]
-}
data ExtMove a = ExtMove a String [a]

-- split an extended move into a non deterministive moves
extMove_to_move :: Ord a => [ExtMove a] -> Set (Move a)
extMove_to_move movesN = fromList $
    [Move p c (fromList q) | 
        (ExtMove p cs q) <- movesN, cs /= "", c <- cs]
    ++ 
    [EMove p (fromList q) | (ExtMove p "" q) <- movesN]
    