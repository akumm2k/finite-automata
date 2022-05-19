import Automaton
import DFA 
import NFA 
import Queue
import Data.List
import Debug.Trace

{-
TODO: implement subset construction
to_dfa :: NFA a -> DFA [a] 
to_dfa (NFA q delta q0 f) = 
    DFA q' delta' q0' f' 
    where 
* Steps:
    Remove lambda transitions
    Build the transitions
    - for each state starting from the [starting states]
        use a queue = [starting state]
        while q
            dequeue q
            find all transitions on each character 
                skip all empty state transitions
            enqueue new states created
    Boom! DFA
-}

subset_constr' :: (Show a, Eq a) => FQueue [a] -> NFA a -> String 
    -> [[a]] -> [Move [a]] -> [Move [a]]
subset_constr' queue _ _ states moves | isEmpty queue = moves 
subset_constr' queue nfa alphabet visited moves = 
    let Just (states, queue') = dequeue queue
        moves' = [Move states c [ps] | c <- alphabet, 
                let ps = concat [ p | q <- states, let p = delta nfa c q,
                        p /= []], ps /= []
            ]
        new_states = concat [ps | Move _ _ ps <- moves']
        visited' = trace ("queue: " ++ show queue ++ "\nvisited" ++
            show visited ++ "\nnew_states: " ++ show new_states ++ "\n") $ 
            states : visited 
        new_queue = foldr enqueue queue' (new_states \\ visited')
    in subset_constr' new_queue nfa alphabet visited' (moves ++ moves')

subset_constr :: (Show a, Eq a) => NFA a -> [Move [a]]
subset_constr n@(NFA q moves q0 f) = 
    let queue = foldr enqueue empty (subsets q)
    in subset_constr' queue n (alphabet_of n) [] []

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = 
    [x : r | r <- rest] ++ rest 
    where rest = subsets xs

{-
test nfa
    L = {w | w[-1] == 1 || w[:-2] == 10}
-}
my_q :: [Int]
my_q = [0 .. 4]
my_q0 :: [Int]
my_q0 = [0] 

my_delta :: [ExtMove Int]
my_delta = [
    ExtMove 0 "0" [0],
    ExtMove 0 "" [2],
    ExtMove 0 "1" [0, 1],
    ExtMove 2 "1" [3],
    ExtMove 3 "0" [1]
    ]

my_f :: [Int]
my_f = [1]

my_nfa :: NFA Int
my_nfa = NFA my_q (extMove_to_move my_delta) my_q0 my_f 

to_nfa :: DFA a -> NFA a 
to_nfa (DFA q delta q0 f) = NFA q delta q0 f 