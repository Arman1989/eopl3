# Recursion in PROC

How can we define a recursive procedure in the PROC language?

```
let sum = proc (n)
             if zero?(n)
             then 0
             else -(n, -(0, (sum -(n, 1))))
in (sum 10)
```

This won't work due to lexical binding. Within the body of the procedure the
environment that gets saved does not contain a binding for `sum`.

**Idea:** *Maybe we can pass the binding for `sum` as an argument to the
procedure.*

```
let makesum = proc (maker)
                proc (n)
                  if zero?(n)
                  then 1
                  else -(n, -(0, ((maker maker) -(n, 1))))
in let sum = (makesum makesum)
   in (sum 10)
```

How cool is that? `sum` has been defined recursively in a language without
explicit support for recursion.

**N.B.** *See `exercise 3.23` were this is introduced.*

We can generalize the trick and show that we can define any recursive procedure
in PROC.

```
let makesum = proc (sum)
                proc (n)
                  if zero?(n)
                  then 0
                  else -(n, -(0, (sum -(n, 1))))
in ...
```

We let `makesum` take the recursive procedure we are trying to make. i.e.

```
let sum = (makesum sum) in ...
```

But this won't work due to lexical binding. So we use the trick to fix it.

```
let makesumrec = proc (maker)
                   (makesum (maker maker))
in let sum = (makesumrec makesumrec)
   in (sum 10)
```

Let's generalize `makesumrec` by taking `makesum` as an argument.

```
let makerec = proc (makef)
                proc (maker)
                  (makef (maker maker))
in let makesumrec = (makerec makesum)
   in let sum = (makesumrec makesumrec)
      in (sum 10)
```

or

```
let makerec = proc (makef)
                let makefrec = proc (maker)
                                 (makef (maker maker))
                in (makefrec makefrec)
in let sum = (makerec makesum)
   in (sum 10)
```
