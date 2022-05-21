import Regular
import System.IO  
import Data.List
import Control.Monad

playNFA :: [String] -> [String] -> IO ()
playNFA fsm test_strs = do 
    let [n, start, final] = fsm 
        -- q = [1 .. read ]
    putStr "String"

playDFA :: [String] -> [String] -> IO ()
playDFA fsm test_strs = do
    putStr "String"

sep :: [Char]
sep = "==="

main :: IO ()
main = do 
    putStrLn "File contains NFA(n) or DFA(d)?"
    answer <- getLine 

    putStrLn "Enter file name: "
    filename <- getLine 
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let l = lines contents
        Just i = elemIndex sep l
        (fsm, test_strs) = splitAt i l
    case answer of 
        "n" -> playNFA fsm test_strs
        "d" -> playDFA fsm test_strs 
        _ -> putStrLn "bad response" 