import Automaton
import NFA 
import DFA 
import Regex
import Regular
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )  
import Data.List as List (splitAt, elemIndex)
import Data.Set ( Set, fromList )

readNMove :: [String] -> Move Int
readNMove (p' : "\\" : q') =
    let p = read p' :: Int 
        q = fromList (read <$> q' :: [Int])
    in EMove p q
readNMove (p' : c' : q') =
    let p = read p' 
        [c] = c' 
        q = fromList (read <$> q' :: [Int])
    in (Move p c q)
readNMove _ = error "bad move syntax"

readDMove :: [String] -> DMove Int 
readDMove (p : [c] : q : []) = 
    DMove (read p) c (read q)
readDMove _ = error "bad move"

accept_str :: (Automaton at, Ord a, Show a) => at a -> String -> String
accept_str at str 
    | at `accepts` str = str ++ " -> accepted"
    | otherwise = str ++ " -> rejected"

play_am :: (Automaton at, Ord a, Show a, Show (at a)) =>
    at a -> [String] -> String -> IO ()
-- given an automaton check if the given strings are recognized by it
play_am am test_strs print_am = do
    let output = unlines $ (accept_str am) <$> test_strs
    case print_am of
        "y" -> putStrLn $ '\n' : show am  ++ "\n\n" ++ output
        "n" -> putStrLn $ '\n' : output
        _ -> putStrLn "bad response."

sep :: String
sep = "==="

play :: String -> IO () 
play filename = do 
    putStrLn "NFA(n) or DFA(d)?"
    answer <- getLine

    putStrLn "Print the automaton? y/n"
    print_am <- getLine

    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let ls = lines contents
        -- split the finite state machine info and test_strs
        Just i = elemIndex sep ls 
        ((n : start' : final' : moves'), (_ : test_strs))
            = splitAt i ls 

        q = fromList [1 .. read n]
        start = fromList (read <$> words start' :: [Int])
        final = fromList (read <$> words final' :: [Int])
        -- moves = fromList ((readMove . words) <$> moves')

    case answer of 
        "n" -> let moves = fromList ((readNMove . words) <$> moves')
            in play_am (build_nfa q moves start final) 
            test_strs print_am
        "d" -> let moves = fromList ((readDMove . words) <$> moves')
            in play_am (build_dfa q moves start final) 
            test_strs print_am
        _ -> putStrLn "bad response"