# Finite Automata and Regular Expressions

![GitHub](https://img.shields.io/github/license/Directoire/under-construction-template)

- [x] Recursive descent parser for regular expressions
- [x] building nfas, and dfas
- [x] converting from nfa -> dfa
- [x] converting from regex -> nfa
- [x] minimizing dfas

## Sample run 

```
$ ghci Regular.hs 
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
...
[1 of 6] Compiling Automaton        ( Automaton.hs, interpreted )
[2 of 6] Compiling DFA              ( DFA.hs, interpreted )
[3 of 6] Compiling NFA              ( NFA.hs, interpreted )
[4 of 6] Compiling Queue            ( Queue.hs, interpreted )
[5 of 6] Compiling Regex            ( Regex.hs, interpreted )
[6 of 6] Compiling Regular          ( Regular.hs, interpreted )
Ok, six modules loaded.

> reg_to_dfa "(0*1|1*0)"
Q: [0,1,2,3,4,5] 
delta: [(0 - 0 -> 3),(0 - 1 -> 5),(1 - 0 -> 1),(1 - 1 -> 4),
(2 - 0 -> 4),(2 - 1 -> 2),(3 - 0 -> 1),(3 - 1 -> 4),(5 - 0 -> 4),
(5 - 1 -> 2)] 
q0: 0 
F: [3,4,5]

> reg_to_dfa "(a?|b*)"
Q: [0,1,2] 
delta: [(0 - a -> 1),(0 - b -> 2),(2 - b -> 2)] 
q0: 0 
F: [0,1,2]

> reg_to_nfa "(a?|b*)"
Q: [0,1,2,3,4,5,6] 
delta: [(0 - \ -> 1, 4),(1 - \ -> 2),(2 - a -> 3),(4 - \ -> 5, 6),
(6 - \ -> 4),(5 - b -> 6)] 
q0: [0] 
F: [1,3,6]
```

## Questions


## Questions Archive

### Automata 

- How to better structure the nfa and dfa implementation, such that
  they implement the same methods for accessing states, delta, start and
  final states?

### Regex Parser

- How to best test the parser?
- Feedback: How to improve it?
- Why might have `Maybe` been useful?
- How to disallow inputs like `??`, and `**`?
- Regexps like `((a|b)*)*` cause an inf recursion in `left_derivative`. How to get rid of it?
    * removing the input str as an arg in the recurivse call works. 
      Is there a more natural way to implement `left_derivative (Star a)`?
- What is the prepending vs appending runtime trade-off in `left_derivative`?
- How to overwrite the implementation of `show` for Data.Set?