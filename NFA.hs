module NFA where 
    
import Automaton
import Data.Set as Set
    ( difference, fromList, member, singleton, 
    toList, union, Set, unions )   
import Data.List ( sort, intercalate )
import Data.Maybe ( fromJust )
{-
* NFA: 
(
    finite set of states        q
    finite set of transitions   delta: Q x Sigma -> P(Q)
    starting states             s0
    set of final states         f
)
-}

data NFA a = 
    NFA {statesN :: Set a, movesN :: [Move a], 
        startN :: Set a, finalN :: Set a} 

emove :: a -> Set a -> Move a
emove a b = ((a, Nothing), b)

nmove :: a -> Char -> Set a -> Move a 
nmove a c b = ((a, Just c), b)

instance (Show a, Ord a) =>  Show (NFA a) where 
    show (NFA q delta s0 f) = 
        "Q: " ++ show (toList q) ++ " \n" ++
        "delta: " ++ del_str ++ " \n" ++ 
        "S0: " ++ show (toList s0) ++ " \n" ++
        "F: " ++ show (toList f) 
        where 
            del_show ((q, Just c), ps) =
                "(" ++ show q ++ " - " ++ [c] ++ " -> " 
                ++ show_states ps ++ ")"
            del_show ((q, Nothing), ps) = 
                "(" ++ show q ++ " - " ++ "\\" ++ " -> " 
                ++ show_states ps ++ ")"
            del_str = intercalate ", " $ del_show <$> (sort delta)


instance Automaton NFA where
    states = statesN
    start = startN 
    final = finalN 
    delta = deltaN
    accepts = acceptsN
    isomorphism = isomorphismN
    alphabet_of = alphabet_of_nfa

build_nfa :: Ord a => 
    Set a -> [Move a] -> Set a -> Set a -> NFA a
build_nfa q delta s0 f = NFA q delta s0 f

{-
delta_star(q, wa) | a :: Char, w :: String
    = epsilon_closure (delta(p, a) for all p in delta_star(q, w))
-}

delta_star :: (Ord a, Show a) =>  NFA a -> String -> Maybe (Set a)
delta_star nfa = delta_star' (startN nfa) nfa

delta_star' :: (Ord a, Show a) => 
    Set a -> NFA a -> String -> Maybe (Set a)
delta_star' fs n [] = Just (unions (set_map (epsilon_closure n) fs))
delta_star' qs nfa (c : cs) = 
    let qs' = unions (set_map (epsilon_closure nfa) qs)
        next = unions (set_map (deltaN nfa c) qs')
    in if set_null next then Nothing else 
        delta_star' next nfa cs

deltaN :: (Show a, Ord a) => NFA a -> Char -> a -> Set a
-- return the transition from p w/ c in nfa
deltaN nfa c p =
    -- pattern match Move to avoid calling `char` on an
    -- epsilon move. char :: (Move a Char a) -> Char
    let qs = unions [s | ((p', Just c'), s) <- movesN nfa, 
            p == p', c' == c]
    in unions [epsilon_closure nfa q | q <- toList qs]

{-
q \in (epsilon_closure q)
if p \in (epsilon_closure q) and EMove p r 
    then r \in (epsilon_closure q)
-}
epsilon_closure' :: (Show a, Ord a) => NFA a -> Set a -> Set a
epsilon_closure' n@(NFA q del s0 f) qs = 
    let new_qs = fromList [r | p <- toList qs, 
            ((p', Nothing), enp) <- del, 
                p == p', r <- toList enp]
    in if set_null (new_qs `difference` qs) then qs 
    else epsilon_closure' n (qs `union` new_qs)

epsilon_closure :: (Show a, Ord a) => NFA a -> a -> Set a
epsilon_closure n q' = 
    epsilon_closure' n (singleton q')

acceptsN :: (Ord a, Show a) => NFA a -> String -> Bool 
acceptsN nfa s = 
    let reached = delta_star nfa s 
    in case reached of 
        Just qs -> or [f `elem` qs | f <- toList $ finalN nfa]
        Nothing -> False

alphabet_of_nfa :: NFA a -> [Char]
alphabet_of_nfa d = rmdups $ 
    [c | ((_, Just c), _) <- movesN d]

{-
let `p \ q` represent an epsilon transition from p to q
if p \ q and p is a starting state, then q is a starting state
if p \ q and q is a final state, then p is a final state

for each `p \ q` elimination, create new move `p c r` such that
there is a move `q c r`
-}
elim_epsilon :: (Show a, Ord a) => NFA a -> NFA a 
elim_epsilon n@(NFA q ms q0 f) = 
    let nq0 =  unions (set_map (epsilon_closure n) q0)
        nf = [p | q <- toList f, ((p, Nothing), qs) <- ms, 
            q `member` qs]
        f' = f `union` (fromList nf )

        deterministic_moves = [((p, Just c), q') | 
            ((p, Just c), q) <- ms,
            let q' = unions (set_map (epsilon_closure n) q)]

        new_moves = [((p, Just c), r) | ((p, Nothing), qs) <- ms, 
            q <- toList $ unions $ set_map (epsilon_closure n) qs, 
            ((q', Just c), r) <- ms, q == q'
            ]
    in NFA q (deterministic_moves ++ new_moves) nq0 f'

isomorphismN :: (Show a, Ord a, Show b, Ord b) => 
    NFA a -> Set b -> NFA b 
-- return an isomorphic NFA with states(NFA) renamed to qs'
isomorphismN n@(NFA q moves s0 f) qs' =
    let qs = states n 
        h = zip (toList qs) (toList qs') -- new labels
        h_each = \x -> fromJust $ lookup x h

        moves' = [((hp, c), hq) | ((p, c), q) <- moves, 
            let Just hp = lookup p h, let hq = (set_map h_each q)]
            ++ 
            [emove hp hq | ((p, Nothing), q) <- moves, 
            let Just hp = lookup p h, let hq = (set_map h_each q)]
        f' = set_map h_each f 
        s0' = set_map h_each s0 

    in NFA qs' moves' s0' f'