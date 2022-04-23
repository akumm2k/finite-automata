data Reg =  
    Epsilon |
    Literal Char |
    Or Reg Reg |        -- (r1|r2)
    Then Reg Reg |      -- r1.r2
    Opt Reg |           -- r?
    Star Reg            -- r*

union_symb :: Char
union_symb = '|' 
star_symb :: Char
star_symb = '*'
is_alpha :: Char -> Bool
is_alpha = \x -> 'a' <= x && x <= 'z'

instance Show Reg where
    show Epsilon = []
    show (Literal c) = [c]
    show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (Then a b) = show a ++ show b 
    show (Opt a) = "(" ++ show a ++ ")" ++ "?"
    show (Star a) = "(" ++ show a ++ ")" ++ "*"

splits :: Int -> Int -> String -> [(String, String)]
splits i j str = 
    [splitAt k str | k <- [i .. j]]

matches :: String -> Reg -> Bool 
matches str Epsilon = str == ""
matches str (Literal c) = str == [c]
matches str (Or a b) = str `matches` a || str `matches` b 
matches str (Then a b) = 
    or [
        s1 `matches` a && s2 `matches` b |
        (s1, s2) <- splits 0 (length str) str
        ]
matches str (Star a) =
    str `matches` Epsilon || -- avoid inf loop
    or [
        s1 `matches` a && s2 `matches` (Star a) |
        (s1, s2) <- splits 1 (length str) str 
        ]
matches str (Opt a) = (str == "") || str `matches` a

{-
* CFG for regular expressions:
char    - 'a' | ... | 'z'
primary - epsilon | char | '(' regexp ')' [Maybe] 

factor - primary | primary '*'
            ~ primary(primary)*
        | primary '?'

term  - factor | term '.' factor 
            ~ factor.(factor)* [Maybe]

regexp    - term | regexp '|' term
---
* Enforced precedence / binding strength in decreasing order:
'*', '?' - iteration, optional
'.' - concat
'|' - union
---
TODO: test parser
-}

primary :: String -> (Reg, Maybe String)
primary ('(' : s) = 
    let (re, Just (')' : t)) = regexp s -- ! returned str must begin w/ `)`
        (c : r) = t
    in 
        if t /= "" 
        then
            case c of
            -- fix for "(aa)*" -> "a(a)*"
            '?' -> (Opt re, Just r) 
            '*' -> (Star re, Just r)
            _ -> (re, Just t)
        else (re, Just t)

primary (c : s) 
    | is_alpha c = (Literal c, Just s)
primary s = (Epsilon, Just s)

factor_ext :: (Reg, Maybe String) -> (Reg, Maybe String)
factor_ext (re, Just "") = (re, Nothing) -- end of str
factor_ext (Then a b, Just ('*' : s)) = 
    -- prime(prime)*
    (Then a (Star b), Just s) 
factor_ext (Then a b, Just ('?' : s)) = (Then a (Opt b), Just s)
factor_ext (re, Just ('*' : s)) = (Star re, Just s)
factor_ext (re, Just ('?' : s)) = (Opt re, Just s)
factor_ext (re, Just (')' : s)) = 
    -- propagate `)` back to regexp_ext
    (re, Just (')' : s))
factor_ext (re, Just s) = 
    let (re2, t) = primary s
        final_opr = (Then re re2, Nothing)
    in 
        if t == Nothing then final_opr
        else
            if Just s == t 
                -- str unchanged -> (head s == ')') is being passed to primary
                then (re, t) 
            else -- carry on parsing
                factor_ext (Then re re2, t) 
factor_ext (re, Nothing) = (re, Nothing)

factor :: String -> (Reg, Maybe String)
factor s =
    let (re, t) = primary s 
    in factor_ext (re, t)

term_ext :: (Reg, Maybe String) -> (Reg, Maybe String)
term_ext (re, Just "") = (re, Nothing) -- end of str
term_ext (re, Just s) =
    let (re2, t) = factor s 
        final_opr = (Then re re2, Nothing)
    in 
        if t == Nothing then final_opr
        else 
            if Just s == t 
                -- str unchanged -> (head s == ')') is being passed to primary
                then (re, t) 
            else -- carry on parsing 
                term_ext (Then re re2, t) 
term_ext (re, Nothing) = (re, Nothing)

term :: String -> (Reg, Maybe String)
term = term_ext . factor

regexp_ext :: (Reg, Maybe String) -> (Reg, Maybe String)
regexp_ext (re, Just "") = (re, Nothing) -- end of str
regexp_ext (re, Just (')' : s)) = 
    -- propagate `)` back to primary
    (re, Just (')' : s)) 
regexp_ext (re, Just ('|' : s)) =
    let (re2, t) = term s 
    in (Or re re2, t)
regexp_ext (re, Nothing) = (re, Nothing)
regexp_ext _ = error "bad syntax"

regexp :: String -> (Reg, Maybe String)
regexp = regexp_ext . term

get_reg :: String -> Reg 
get_reg s = 
    let (r, str) = regexp s 
    in
    if str == Nothing then r else error "something went wrong"

test_reg :: [String]
test_reg = 
    let test_strs = ["(aa)*", "a(aa)*", "*a?", "xy(a?|b*c)cd"]  
        test_strs2 = ["a??", "a**", "(a*)*", "?a??"]
    in [re ++ " -> " ++ show (get_reg re) | re <- test_strs ++ test_strs2]