# Finite Automata and Regular Expressions

![GitHub](https://img.shields.io/github/license/Directoire/under-construction-template)

- [x] Recursive descent parser for regular expressions
- [x] building nfas, and dfas
- [x] converting from nfa -> dfa
- [x] converting from regex -> nfa
- [x] minimizing dfas
- [x] feature to test from files

## Sample run 

```
$ ghci filetest.hs 
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
...
[1 of 6] Compiling Automaton        ( Automaton.hs, interpreted )
[2 of 6] Compiling DFA              ( DFA.hs, interpreted )
[3 of 6] Compiling NFA              ( NFA.hs, interpreted )
[4 of 6] Compiling Queue            ( Queue.hs, interpreted )
[5 of 6] Compiling Regex            ( Regex.hs, interpreted )
[6 of 6] Compiling Regular          ( Regular.hs, interpreted )
Ok, six modules loaded.

> "hello" `matches` "hello?"
True
> "huh" `matches` "(this)?(is)*(it)?huh?"
True
> "isisisisish" `matches` "(this)?(is)*(it)?huh?"
False
> "bananananananananana" `matches` "bana(na)*"
True

> reg_to_dfa "(a?|b*)"
Q: [0,1,2] 
delta: [(0 - a -> 1),(0 - b -> 2),(2 - b -> 2)] 
q0: 0 
F: [0,1,2]

> reg_to_dfa "(0*1|1*0)"
Q: [0,1,2,3,4,5] 
delta: [(0 - 0 -> 3),(0 - 1 -> 5),(1 - 0 -> 1),(1 - 1 -> 4),
(2 - 0 -> 4),(2 - 1 -> 2),(3 - 0 -> 1),(3 - 1 -> 4),(5 - 0 -> 4),
(5 - 1 -> 2)] 
q0: 0 
F: [3,4,5]

> reg_to_nfa "(a?|b*)"
Q: [0,1,2,3,4,5,6] 
delta: [(0 - \ -> 1, 4),(1 - \ -> 2),(2 - a -> 3),(4 - \ -> 5, 6),
(6 - \ -> 4),(5 - b -> 6)] 
S0: [0] 
F: [1,3,6]

> n = reg_to_nfa "(0|1)*(01|10)"
> n
Q: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] 
delta: [(1 - \ -> 2, 4, 6),(2 - \ -> 3, 5),(4 - \ -> 7),(4 - \ -> 1),
(6 - \ -> 1),(3 - 0 -> 4),(5 - 1 -> 6),(6 - \ -> 7),(7 - \ -> 8, 12),
(8 - 0 -> 9),(9 - \ -> 10),(10 - 1 -> 11),(12 - 1 -> 13),(13 - \ -> 14),
(14 - 0 -> 15)] 
S0: [1] 
F: [11,15]
> n `accepts` "10010"
True
> n `accepts` "10000"
False
> n `accepts` "100001111"
False
> n `accepts` "100001"
True
> n `accepts` ((replicate 10000 '1') ++ "01")
True
```

* NFA to DFA conversion is extremely inefficient - $O(n ^ 3 2 ^ n)$. This is because we must look at all subsets of the states of the NFA in a process called `Subset Construction`.
* `reg_to_dfa` is essentially the composition `nfa_to_dfa . reg_to_nfa`.

* Regex to NFA is $O(n)$.

### DFA Minimization

$\in \ O(n ^ 4)$

```
$ ghci tests/dfa-test.hs 
...
> d2
Q: [0,1,2,3] 
delta: [(0 - 0 -> 0),(0 - 1 -> 1),(1 - 0 -> 1),(1 - 1 -> 2),
(2 - 0 -> 2),(2 - 1 -> 3),(3 - 0 -> 3),(3 - 1 -> 0)] 
q0: 0 
F: [0,2]
> minimize d2
Q: [0,1] 
delta: [(0 - 0 -> 0),(0 - 1 -> 1),(1 - 0 -> 1),(1 - 1 -> 0)] 
q0: 0 
F: [0]
```

$L(d2) =$ { w | w $\in$  {0, 1}, |w|$_1$ = 2k, k $\in$ {$0 \ ..\ \infty$} }

## Running tests from a file 

`nfa.txt`
```
3
0
2
0 a 0
0 a 1
0 b 0
1 b 2
2 a 0
2 a 2
2 b 1
===
aaba
bba
```
* The first three lines describe the number of states, starting states, and final states respectively. Then come the transitions of the form `p c qs`, where $\delta$(p, c) = {qs}. For a lambda / epsilon transition, a `/` must be used. For instance, `0 / 1 2` describes a lambda transition from 0 to 1 and 2.
* `===` is the separator.
* Then, there are test strings, one per line.

### Sample file-test run 

* `play` takes a filename as an argument.
```
$ ghci filetest.hs
...
> play "tests/nfa.txt"
NFA(n) or DFA(d)?
n
Print the automaton? y/n
y

Q: [1,2,3] 
delta: [(0 - a -> 0),(0 - a -> 1),(0 - b -> 0),(1 - b -> 2),
(2 - a -> 0),(2 - a -> 2),(2 - b -> 1)] 
S0: [0] 
F: [2]

aaba -> accepted
bba -> rejected
```

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