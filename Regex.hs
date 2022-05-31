module Regex where 

data Reg =  
    Epsilon |
    Literal Char |
    Or Reg Reg |        -- (r1|r2)
    Then Reg Reg |      -- r1.r2
    Opt Reg |           -- r?
    Star Reg            -- r*
    deriving (Eq)
    
instance Show Reg where
    show Epsilon        = []
    show (Literal c)    = [c]
    show (Or a b)       = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (Then a b)     = show a ++ show b 
    show (Opt a)        = "(" ++ show a ++ ")" ++ "?"
    show (Star a)       = "(" ++ show a ++ ")" ++ "*"

{-
* CFG for regular expressions:
char -      'a' | ... | 'z'
primary -   epsilon | char | '(' '.' regexp '.' ')'

factor -    primary | factor '.' primary ~ primary '.' (primary) '*' 
            | primary '*' | primary '?'

term -      factor | term '.' factor     ~ factor '.' (factor) '*' 

regexp -    term | regexp '|' term
---
* Enforced precedence / binding strength in decreasing order:
'*', '?' - iteration, optional
'.' - concatenation
'|' - union
-}

valid_char :: Char -> Bool
-- valid ascii range: [33, 126]
valid_char = \x -> '!' <= x && x <= '~' 
    && not (x `elem` "()|*\\")

primary :: String -> (Reg, String)
primary ('(' : s) = 
    let (re, t) = regexp s in 
    if t /= "" && head t == ')' then 
        primary_process (re, tail t)
    else error "bad syntax"

primary (c : s) | valid_char c = (Literal c, s)
primary s = (Epsilon, s)

primary_process :: (Reg, String) -> (Reg, String)
primary_process (re, t) = 
    let (c : r) = t in
    if t == "" then (re, t)
    else
        case c of
        -- fix for "(aa)*" -> "a(a)*"
        '?' -> (Opt re, r) 
        '*' -> (Star re, r)
        _   -> (re, t)


-- factor - primary | factor . primary | primary '*' | primary '?'
factor :: String -> (Reg, String)
factor = factor_ext . primary

{-
factor_ext recursively builds a factor that can't be 
broken by primary concatenation, iteration or the optional operator.
-}
factor_ext :: (Reg, String) -> (Reg, String)

-- '*' and '?' bind to the last reg
factor_ext (Then a b, ('*' : s)) = (Then a (Star b), s) 
factor_ext (Then a b, ('?' : s)) = (Then a (Opt b), s)
factor_ext (re, ('*' : s)) = (Star re, s)
factor_ext (re, ('?' : s)) = (Opt re, s)

factor_ext (re, (')' : s)) = 
    -- propagate `)` back to regexp_ext
    (re, (')' : s))

factor_ext (re, s) = 
    let (re2, t) = primary s in 
    if s == t 
        -- str unchanged -> (head s == ')') to be passed to primary
        then (re, t) 
    else -- carry on parsing
        factor_ext (Then re re2, t) -- primary.(primary)*


-- term - factor | term '.' factor 
term :: String -> (Reg, String)
term s = 
    let (re, t) = factor s
    in term_ext (re, t)

{-
term_ext recursively builds a term that can't be 
broken by factor concatenation.
-}
term_ext :: (Reg, String) -> (Reg, String)
term_ext (re, s) =
    let (re2, t) = factor s 
    in 
        if s == t 
        -- str unchanged -> (head s == ')') to be passed to primary
            then (re, t) 
        else -- carry on parsing 
            term_ext (Then re re2, t) -- factor.(factor)* 


-- regexp -    term | regexp '|' term
regexp :: String -> (Reg, String)
regexp = regexp_ext . term

{-
regexp_ext helps build a regexp that can't be 
broken by the union '|' opr
-}
regexp_ext :: (Reg, String) -> (Reg, String)
regexp_ext (re, (')' : s)) = 
    -- propagate `)` back to primary
    (re, (')' : s)) 
regexp_ext (re, ('|' : s)) =
    let (re2, t) = term s 
    in regexp_ext (Or re re2, t)
regexp_ext (re, "") = (re, "")
regexp_ext _ = error "bad syntax"

-- get_reg str: parse str to get a Reg value
get_reg :: String -> Reg 
get_reg s = 
    let (re, str) = regexp s in
    if str == "" then re 
    else error 
        ("something went wrong. Unconsumed substring: " ++ str)

{-
left_derivative [re :: Reg] s =
    L(re) \ {s} =
    [v | uv = s and u in L(re)]
        v = "" <-> uv = u = s in L(re) <-> s `matches` re
-}
left_derivative :: Reg -> String -> [String]
left_derivative Epsilon s = [s]

left_derivative (Literal c) (d : s) | (c == d) = [s]
left_derivative (Literal c) s = []

left_derivative (Or a b) s = 
    left_derivative a s ++ left_derivative b s 
left_derivative (Then a b) s = 
    [u | t <- left_derivative a s, u <- left_derivative b t] 

left_derivative (Opt a) s = left_derivative a s ++ [s]
left_derivative (Star a) s = 
    let m = remove_item s (left_derivative a s) 
        -- rm s to prevent inf recursion
    in concat (map (left_derivative (Star a)) m) ++ [s]

-- remove all occurences of an item in a list
remove_item :: Eq a => a -> [a] -> [a]
remove_item _ [] = []
remove_item x (y : ys) 
    | x == y    = remove_item x ys
    | otherwise = y : remove_item x ys

{-
get_left_derivative [re :: String] str
    `left_derivative` but takes a regex `re` as string
    and applies `left_derivative` to the parsed regex
-}
get_left_derivative :: String -> String -> [String]
get_left_derivative re = left_derivative (get_reg re)

{-
str `matches` re
    if `str` matches the regex string `re`
    * Recall: "" in left_derivative re str -> str was matched
-}
matches :: String -> String -> Bool 
matches s re_str = 
    let re = get_reg re_str
    in "" `elem` (left_derivative re s)

-- test_reg_parse: test if the precedence is enforeced correctly 
test_reg_parse :: [String]
test_reg_parse = 
    let test_strs = ["(aa)*", "a(aa)*", "*a?", "xy(a?|b*c)cd"]
        test_strs2 = ["a??", "a**", "(a*)*", "?a??", "axy(bc?d*|)g"]
    in [re ++ " -> " ++ show (get_reg re) | 
        re <- test_strs ++ test_strs2]