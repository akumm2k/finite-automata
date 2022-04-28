type NFA a = ([a], [Char], a -> Char -> [a], a, a -> Bool)

lambda_closure :: Eq a => a -> NFA a -> [a]
lambda_closure q' (q, sigma, delta, q0, f) = 
    q' : (delta q' '/')

{-
? How do we define the transition function when building an NFA ?
-}