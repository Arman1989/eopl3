# Essentials of Programming Languages (3rd Edition)

My notes and solutions to exercises.

## Tips

**How to unregister a package**

```
$ stack exec ghc-pkg unregister <package-name>
```

I needed to know how to unregister a package in order to solve the following
issue:

```
Prelude> import Let

<no location info>: error:
    Ambiguous module name ‘Let’:
      it was found in multiple packages: let-0.1.0.0 let-extended-0.1.0.0
```

Ref: https://stackoverflow.com/a/38639959/391924.

## Resources

- [Essentials of Programming Languages: 3rd Edition](http://www.eopl3.com/)
