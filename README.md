# Finite Automata and Regular Expressions

![GitHub](https://img.shields.io/github/license/Directoire/under-construction-template)

- [x] Recursive descent parser for regular expressions
- [ ] building nfas, and dfas
- [ ] converting from nfa -> dfa
- [ ] converting from regex -> nfa
- [ ] minimizing dfas

## Questions



## Questions Archive


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