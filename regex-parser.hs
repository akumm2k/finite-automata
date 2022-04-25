import Data.List
data Reg =  
    Epsilon |
    Literal Char |
    Or Reg Reg |        -- (r1|r2)
    Then Reg Reg |      -- r1.r2
    Opt Reg |           -- r?
    Star Reg            -- r*

instance Show Reg where
    show Epsilon = []
    show (Literal c) = [c]
    show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (Then a b) = show a ++ show b 
    show (Opt a) = "(" ++ show a ++ ")" ++ "?"
    show (Star a) = "(" ++ show a ++ ")" ++ "*"

{-
matches_of re s =
    [v | uv = s and u `matches` re]
        v = "" -> s `matches` re
-}
matches_of :: Reg -> String -> [String]
matches_of Epsilon s = [s]

matches_of (Literal c) (d : s) | (c == d) = [s]
matches_of (Literal c) s = []

matches_of (Or a b) s = matches_of a s ++ matches_of b s 
matches_of (Then a b) s = [u | t <- matches_of a s, u <- matches_of b t] 

matches_of (Opt a) s = matches_of a s ++ [s]
matches_of (Star a) s = 
    let m = matches_of a s
    in concat (map (matches_of (Star a)) m) ++ [s]

get_matches_of :: String -> String -> [String]
get_matches_of re str = matches_of (get_reg re) str

matches :: String -> String -> Bool 
matches s re_str = 
    let re = get_reg re_str
        m = matches_of re s
    in not (null m) && "" `elem` m

{-
* CFG for regular expressions:
char -      'a' | ... | 'z'
primary -   epsilon | char | '(' regexp ')'

factor -    primary | primary '*' ~ primary(primary)* | primary '?'

term -      factor | term '.' factor ~ factor.(factor)* 

regexp -    term | regexp '|' term
---
* Enforced precedence / binding strength in decreasing order:
'*', '?' - iteration, optional
'.' - concat
'|' - union
-}

is_alpha :: Char -> Bool
is_alpha = \x -> 'a' <= x && x <= 'z'

primary :: String -> (Reg, String)
primary ('(' : s) = 
    let (re, t) = regexp s in 
    if t /= "" && head t == ')' then 
        primary_process (re, tail t)
    else error "bad syntax"

primary (c : s) | is_alpha c = (Literal c, s)
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
        _ -> (re, t)


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


regexp :: String -> (Reg, String)
regexp = regexp_ext . term

{-
regexp_ext helps build a regexp that can't be broken by the union '|' operator
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


get_reg :: String -> Reg 
get_reg s = 
    let (re, str) = regexp s in
    if str == "" then re 
    else error ("something went wrong. Unconsumed substring:" ++ str)

test_reg_parse :: [String]
test_reg_parse = 
    let test_strs = ["(aa)*", "a(aa)*", "*a?", "xy(a?|b*c)cd"]
        test_strs2 = ["a??", "a**", "(a*)*", "?a??", "axy(bc?d*|)g"]
    in [re ++ " -> " ++ show (get_reg re) | re <- test_strs ++ test_strs2]