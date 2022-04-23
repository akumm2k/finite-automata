data Reg =  
    Epsilon |
    Literal Char |
    Or Reg Reg |        -- (r1|r2)
    Then Reg Reg |      -- r1.r2
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

{-
CFG:
char    - 'a' | ... | 'z'

primary - eps | char | '(' expr ')' [Maybe] 

factor - primary | primary '*'

term  - factor | term '.' factor 
            ~ factor.(factor)* [Maybe]

expr    - term | expr '|' term

---

TODO: fix parser
-}

primary :: String -> (Reg, Maybe String)
primary ('(' : s) = 
    let (re, Just (')' : t)) = regexp s 
    in (re, Just t)
primary (c : s) 
    | is_alpha c = (Literal c, Just s)
primary s = (Epsilon, Just s)

factor_ext :: (Reg, String) -> (Reg, Maybe String)
factor_ext (re, "") = (re, Nothing)
factor_ext (Then a b, '*' : s) = (Then a (Star b), Just s)
factor_ext (re, '*' : s) = (Star re, Just s)
factor_ext (re, ')' : s) = (re, Just (')' : s))
factor_ext (re, s) = 
    let (re2, Just t) = primary s
    in if s == t then (re, Just t) 
        else factor_ext (Then re re2, t)

factor :: String -> (Reg, Maybe String)
factor s =
    let (re, Just t) = primary s 
    in factor_ext (re, t)

term_ext :: (Reg, Maybe String) -> (Reg, Maybe String)
term_ext (re, Just "") = (re, Nothing)
term_ext (re, Just s) =
    let (re2, Just t) = primary s 
    in if s == t then (re, Just t) 
    else term_ext (Then re re2, Just t)
term_ext (re, Nothing) = (re, Nothing)

term :: String -> (Reg, Maybe String)
term = term_ext . factor

regexp_ext :: (Reg, Maybe String) -> (Reg, Maybe String)
regexp_ext (re, Just "") = (re, Nothing)
regexp_ext (re, Just (')' : s)) = (re, Just (')' : s))
regexp_ext (re, Just ('|' : s)) =
    let (re2, t) = term s 
    in (Or re re2, t)
regexp_ext (re, Nothing) = (re, Nothing)
regexp_ext _ = error "bad syntax"

regexp :: String -> (Reg, Maybe String)
regexp = regexp_ext . term

get_reg :: String -> Reg 
get_reg s = r 
    where (r, _) = regexp s