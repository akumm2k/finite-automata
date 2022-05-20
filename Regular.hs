module Regular where 

import Automaton
import DFA 
import NFA 
import Regex
import Queue
import Data.List as List hiding (union)
import Data.Set as Set 

{-
Reg to nfa
    r -> NFA q delta s0 f
-}
reg_to_nfa :: String -> NFA Int 
reg_to_nfa = reg_to_nfa' . get_reg 

reg_to_nfa' :: Reg -> NFA Int
reg_to_nfa' Epsilon = NFA z Set.empty z z
    where z = singleton 0
    -- Epsilon = NFA [0] [] [0] [0]

reg_to_nfa' (Literal c) = NFA q ms (singleton 0) (singleton 1)
    where 
        q = (fromList [0, 1])
        ms = singleton (Move 0 c (singleton 1))
    -- Literal c = NFA [0, 1] [0 - c -> 1] [1]

reg_to_nfa' (Or r1 r2) = 
    --  Or r1 r2: new start -\-> [start n1, start n2]
    let (l1, l2, n1', n2') = regs_to_distinct_nfa r1 r2
        emove = singleton $ EMove 0 (start n1' `union` start n2')
    in (NFA (fromList [0 .. l1 + l2])
        (emove `union` (moves n1' `union` moves n2')) 
        (singleton 0) (final n1' `union` final n2'))

reg_to_nfa' (Then r1 r2) = 
    -- Then r1 r2: final n1 -\-> start n2
    let (l1, l2, n1', n2') = regs_to_distinct_nfa r1 r2
        emoves = fromList 
            [EMove en1 (start n2') | en1 <- toList $ final n1']
    in (NFA (fromList [1 .. l1 + l2]) 
        (emoves `union` moves n1' `union` moves n2')
        (start n1') (final n2'))

reg_to_nfa' (Opt r) = 
    -- Opt r1: new start -\-> start n1 | new start \in final
    let n = reg_to_nfa' r 
        l = length $ states n
        n' = isomorphism n (fromList [1 .. l])
        emove = singleton $ EMove 0 (start n')
    in NFA (fromList [0 .. l]) (emove `union` moves n') 
        (singleton 0) (singleton 0 `union` final n')

reg_to_nfa' (Star r) = 
    -- Star r1: new start <-\-> final n1, and new start -\-> start n1
    let n = reg_to_nfa' r 
        l = length $ states n
        n' = isomorphism n (fromList [1 .. l])
        emoves = fromList ([EMove 0 (start n' `union` final n')] 
            ++ [EMove f (singleton 0) | f <- toList $ final n'])
    in NFA (fromList [0 .. l]) 
        (emoves `union` moves n') (singleton 0) (final n')

regs_to_distinct_nfa :: Reg -> Reg -> (Int, Int, NFA Int, NFA Int)
-- given two regs, get their corresponding distinct NFAs w/ their lengths
regs_to_distinct_nfa r1 r2 = 
    let n1 = reg_to_nfa' r1 
        n2 = reg_to_nfa' r2
         -- map the states to distinct ints
    in differentiate_states n1 n2

{-
all states in the DFA are converted to sigleton sets
in the NFA. DFAs are internally implemented with singleton sets
so we can convert them to NFAs directly.
-}
to_nfa :: DFA a -> NFA a 
to_nfa (DFA q delta q0 f) = NFA q delta q0 f 

{-
* NFA to DFA:
    Remove lambda transitions
    subset_constr
    Boom! DFA

for an NFA (Q, delta, S0, F)
the following is the equivalent DFA
(P(Q), delta', {S0}, {q | q intersetion f is not null for q in P(Q)})
-}
to_dfa :: (Ord a, Show a) => NFA a -> DFA (Set a)
to_dfa n = 
    DFA (subsets q) delta' (singleton s0) f'
    where 
        n'@(NFA q delta s0 f) = elim_epsilon n
        delta' = subset_constr n'
        f' = fromList $  
            ([p' | (Move _ _ p) <- toList delta', let [p'] = toList p, 
            intersection p' f /= Set.empty]
            ++ 
            [q | (Move q _ _) <- toList delta', 
                intersection q f /= Set.empty])

{-
* Subset Construction for nfa n
Build new transitions
    - for each state in queue starting with those in the Power Set of s0
        while queue
            s <- dequeue queue
            find all transitions on each character from each state in s
                skip all empty state transitions
            enqueue new states created

subset_constr' :: queue nfa alphabet states visited_states 
    constructed_moves_so_far
-}
subset_constr' :: (Show a, Ord a) => FQueue (Set a) -> NFA a -> String 
    -> [Set a] -> Set (Move (Set a)) -> Set (Move (Set a))
subset_constr' queue _ _ states moves | isEmpty queue = moves 
subset_constr' queue nfa alphabet visited moves = 
    let Just (states, queue') = dequeue queue
        moves' = fromList [Move states c (singleton ps) | c <- alphabet, 
                let ps =  fromList $ listSetCat [p | q <- toList states, 
                        let p = delta nfa c q, p /= Set.empty], 
                    ps /= Set.empty
            ]
        new_states = fromList $ listSetCat [ps | Move _ _ ps <- toList moves']
        visited' = states : visited 
        new_queue = List.foldr enqueue queue' ((toList new_states) List.\\ visited')
    in subset_constr' new_queue nfa alphabet visited' (moves `union` moves')

subset_constr :: (Show a, Ord a) => NFA a -> Set (Move (Set a))
subset_constr n@(NFA q moves q0 f) = 
    let queue = List.foldr enqueue Queue.empty (subsets q)
    in subset_constr' queue n (alphabet_of n) [] Set.empty


subsets :: Set a -> Set (Set a)
subsets = powerSet 