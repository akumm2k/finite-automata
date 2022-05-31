module Regular where 

import Debug.Trace
import Automaton
import DFA 
import NFA 
import Regex 
import Queue 
import Data.List as List ( (\\) )
import Data.Set as Set
    ( empty, fromList, intersection, powerSet, singleton,
      toList, union, Set, unions, difference )

reg_to_dfa :: String -> DFA Int
reg_to_dfa = minimize . nfa_to_dfa . reg_to_nfa 

{-
Reg to nfa
    r -> NFA q delta s0 f
-}
reg_to_nfa :: String -> NFA Int 
reg_to_nfa s = nfa
    where 
        r = get_reg s
        (nfa, _) = reg_to_nfa' r 0

reg_to_nfa' :: Reg -> Int -> (NFA Int, Int)
reg_to_nfa' Epsilon i = (NFA z empty_set z z, i + 1)
    where z = singleton i
    -- Epsilon = NFA [i] [] [i] [i]

reg_to_nfa' (Literal c) i = 
        (NFA q ms (singleton i) (singleton j), j + 1)
    where 
        j = i + 1
        q = (fromList [i, j])
        ms = singleton (nmove i c (singleton j))
    -- Literal c = NFA [i, j] [i - c -> j] [j]

reg_to_nfa' (Or r1 r2) i = 
    --  Or r1 r2: new start -\-> [start n1, start n2]
    let (n1', i') = reg_to_nfa' r1 (i + 1)
        (n2', i'') = reg_to_nfa' r2 i'
        eps_move = singleton $ emove i (start n1' `union` start n2')
        nfa = NFA (fromList [i + 1 .. i'' - 1])
                (unions [eps_move, movesN n1', movesN n2']) 
                (singleton i) (final n1' `union` final n2')
        in (nfa, i'')

reg_to_nfa' (Then r1 r2) i = 
    -- Then r1 r2: final n1 -\-> start n2
    let (n1', i') = reg_to_nfa' r1 i
        (n2', i'') = reg_to_nfa' r2 i'
        emoves = fromList 
            [emove en1 (start n2') | en1 <- toList $ final n1']
    in 
        (NFA (fromList [i .. i'' - 1]) 
            (emoves `union` movesN n1' `union` movesN n2')
            (start n1') (final n2'), i'')

reg_to_nfa' (Opt r) i = 
    -- Opt r1: new start -\-> start n1 | new start \in final
    let (n, i') = reg_to_nfa' r (i + 1)
        eps_move = singleton $ emove i (start n)
    in (NFA (fromList [i .. i' - 1]) (eps_move `union` movesN n) 
        (singleton i) (singleton i `union` final n), i')

reg_to_nfa' (Star r) i = 
    -- Star r1: new start <-\-> final n1, new start -\-> start n1
    let (n, i') = reg_to_nfa' r (i + 1)
        emoves = fromList ([emove i (start n `union` final n)] 
            ++ [emove f (singleton i) | f <- toList $ final n])
    in (NFA (fromList [i .. i' - 1]) (emoves `union` movesN n) 
        (singleton i) (final n), i')

{-
all states in the DFA are converted to sigleton sets
in the NFA. DFAs are internally implemented with singleton sets
so we can convert them to NFAs directly.
-}
dfa_to_nfa :: (Ord a) => DFA a -> NFA a 
dfa_to_nfa (DFA q delta q0 f) = NFA q delta' q0 f 
    where
        delta' = fromList
            [nmove p c (singleton q) | (DMove p c q) <- toList delta]
    

{-
* NFA to DFA:
    Remove lambda transitions
    subset_constr
    Boom! DFA

for an NFA (Q, delta, S0, F)
the following is the equivalent DFA
(P(Q), delta', {S0}, {q | q intersetion f isn't null for q in P(Q)})
TODO: new approach
    start from the new starting state,
    do a dfs to find reachable states, using a 
    modified ajacent function
-}
nfa_to_dfa :: (Ord a, Show a) => NFA a -> DFA (Set a)
nfa_to_dfa n = 
    DFA pq delta' (singleton s0) f'
    where 
        n'@(NFA q delta s0 f) = elim_epsilon n
        (pq, delta') = subset_constr n'
        f' = fromList $ 
            [q | q <- toList pq, q `intersection` f /= empty_set]

{-
* Subset Construction for nfa n: BFS
Build new transitions 
    - for each state in queue starting with those 
    in the starting state of the new dfa: s0
        while queue
            s <- dequeue queue
            find all transitions on each character 
            from each state in s
                skip all empty state transitions
            enqueue new states created
-}
subset_constr :: (Show a, Ord a) => 
    NFA a -> (Set (Set a), Set (DMove (Set a)))
subset_constr n@(NFA q moves s0 f) = 
    let queue = enqueue s0 empty_queue 
    in subset_constr' queue n (alphabet_of n) (singleton s0)  empty_set

{-
subset_constr' :: queue nfa alphabet states visited_states 
    constructed_moves_so_far
-}
subset_constr' :: (Show a, Ord a) => 
    FQueue (Set a) -> NFA a -> String -> Set (Set a) -> 
    Set (DMove (Set a)) -> (Set (Set a), Set (DMove (Set a)))
subset_constr' queue _ _ visited moves 
    | isEmpty queue = (visited, moves)

subset_constr' queue nfa alphabet visited moves = 
    let Just (states, queue') = dequeue queue
        new_moves_n_states =  
            [(DMove states c ps, ps) | 
            c <- alphabet, let ps = unions 
                                [p | q <- toList states, 
                                let p = delta nfa c q, 
                                p /= empty_set], 
            ps /= empty_set
            ] -- compute new transitions skipping empty ones
        new_states = fromList $ snd <$> new_moves_n_states
        moves' = fromList $ fst <$> new_moves_n_states

        visited' = 
             (singleton states) `union` visited
        
        new_queue = foldr enqueue queue' 
            (new_states `difference` visited')
    in 
        (subset_constr' new_queue nfa alphabet 
            visited' (moves `union` moves'))

empty_queue :: FQueue a
empty_queue = Queue.empty