```
ExpVal = Int + Bool + List[ExpVal]
DenVal = ExpVal


Expression ::= cons (Expression, Expression)
               [(cons-exp exp1 exp2)]

           ::= car (Expression)
               [(car-exp exp)]

           ::= cdr (Expression)
               [(cdr-exp exp)]

           ::= null? (Expression)
               [(null?-exp exp)]

           ::= emptylist
               [(emptylist-exp)]


list-val     : List[ExpVal] -> ExpVal
expval->list : ExpVal -> List[ExpVal]


empty : List[ExpVal]
cons  : ExpVal -> ExpVal -> List[ExpVal]
car   : List[ExpVal] -> ExpVal
cdr   : List[ExpVal] -> ExpVal
null? : List[ExpVal] -> Bool


(value-of (cons-exp exp1 exp2) ρ)
= (list-val (cons (value-of exp1 ρ) (value-of exp2 ρ)))

(value-of (car-exp exp) ρ)
= (car (expval->list (value-of exp ρ)))

(value-of (cdr-exp exp) ρ)
= (cdr (expval->list (value-of exp ρ)))

(value-of (null?-exp exp) ρ)
= (bool-val (null? (expval->list (value-of exp ρ))))

(value-of (emptylist-exp) ρ)
= (list-val empty)
```
