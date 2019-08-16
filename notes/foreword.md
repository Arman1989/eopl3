# Foreword

The most fundamental idea in computer programming is that *the interpreter for a
computer language is just another program*.

Mastering the idea of an interpreter is a source of great power.

If you don't understand interpreters, you can still write programs; you can even
be a competent programmer. But you can't be a master.

Three reasons why you should learn about interpreters:

1. You will need to implement interpreters at some point.
2. Even programs that are not themselves interpreters have important
   interpreter-like pieces. One of the most powerful ways to structure a complex
   program is as a collection of languages.
3. Programming techniques that explicitly involve the structure of language are
   becoming increasingly important.

The core of the book presents a sequence of interpreters starting with an
abstract high-level language and progressively making linguistic features
explicit until we reach a state machine.

The authors show how the same ideas can be used to analyze programs without
running them.

This is not an easy book. The language designer is a further level removed from
the end user than is the ordinary application programmer.

Interpreters are subtle programs. Don't think that you can just skim these
programs--very few people in the world can glance at a new interpreter and
predict from that how it will behave even on relatively simple programs. Study
these programs. Run them. Try to really master these programs, not just get a
vague feeling for how they work.

Creating new frameworks requires skills of the master:

- Understanding the principles that run across languages
- Appreciating which language features are best suited for which type of
  application
- Knowing how to craft the interpreters that bring these languages to life
