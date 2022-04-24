data Reg =  
    Epsilon |
    Literal Char |
    Or Reg Reg |        -- (r1|r2)
    Then Reg Reg |      -- r1.r2
    Opt Reg |           -- r?
    Star Reg            -- r*

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

primary :: String -> (Reg, String)
primary ('(' : s) = 
    let (re, t) = regexp s in 
    if t /= Nothing then 
        let Just t' = t in 
            if head t' == ')' then
                primary_process (re, tail t')
            else error "bad syntax"
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


regexp :: String -> (Reg, Maybe String)
regexp s = 
    let (re, t) = term s
    in regexp_ext (re, Just t)

{-
regexp_ext helps build a regexp that can't be broken by the union '|' operator
-}
regexp_ext :: (Reg, Maybe String) -> (Reg, Maybe String)
regexp_ext (re, Just "") = (re, Nothing) -- end of str
regexp_ext (re, Just (')' : s)) = 
    -- propagate `)` back to primary
    (re, Just (')' : s)) 
regexp_ext (re, Just ('|' : s)) =
    let (re2, t) = term s 
    in (Or re re2, Just t)
regexp_ext (re, Nothing) = (re, Nothing)
regexp_ext _ = error "bad syntax"


get_reg :: String -> Reg 
get_reg s = 
    let (re, str) = regexp s in
    if str == Nothing then re 
    else error "something went wrong"

test_reg :: [String]
test_reg = 
    let test_strs = ["(aa)*", "a(aa)*", "*a?", "xy(a?|b*c)cd"]
        test_strs2 = ["a??", "a**", "(a*)*", "?a??", "axy(bc?d*|)g"]
    in [re ++ " -> " ++ show (get_reg re) | re <- test_strs ++ test_strs2]