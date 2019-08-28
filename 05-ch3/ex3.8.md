```
Expression ::= equal? (Expression, Expression)
               [(equal?-exp exp1 exp2)]

           ::= greater? (Expression, Expression)
               [(greater?-exp exp1 exp2)]

           ::= less? (Expression, Expression)
               [(less?-exp exp1 exp2)]


     (value-of exp1 ρ) = val1
     (value-of exp2 ρ) = val2
-----------------------------------
(value-of (equal?-exp exp1 exp2) ρ)
= (bool-val #t) if (expval->num val1) == (expval->num val2)
= (bool-val #f) if (expval->num val1) /= (expval->num val2)

     (value-of exp1 ρ) = val1
     (value-of exp2 ρ) = val2
-------------------------------------
(value-of (greater?-exp exp1 exp2) ρ)
= (bool-val #t) if (expval->num val1) > (expval->num val2)
= (bool-val #f) if (expval->num val1) <= (expval->num val2)

    (value-of exp1 ρ) = val1
    (value-of exp2 ρ) = val2
----------------------------------
(value-of (less?-exp exp1 exp2) ρ)
= (bool-val #t) if (expval->num val1) < (expval->num val2)
= (bool-val #f) if (expval->num val1) >= (expval->num val2)
```
