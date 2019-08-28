```
Expression ::= minus (Expression)
               [(minus-exp exp)]


(value-of (minus-exp exp) ρ)
= (num-val (- (expval->num (value-of exp ρ))))
```
