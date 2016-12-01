Design goals for Chick
======================

Favor users, abandon hopes for efficiency
-----------------------------------------

I guess the major goal is to make the programming/proving experience much more easy/rich, by exposing a lot more of the internals and keeping track of as much information as possible. This will most likely result in excruciating performance, but this is part of the exercise.

Uniquely-indexable subterms
---------------------------

The language may have a couple of different term levels:

- post-parsing (looks like what the user wrote)
  
  `fun x y => eqb x y`

- post-implicits-insertion

  `fun x y => @eqb _ _ x y`

- post-typing

  `fun x y => @eqb (?T: Type) (?eq: Eq ?T) x y`

- post-unification

  `fun x y => @eqb bool (?eq: Eq bool) x y`

- post-typeclass-resolution (fully-elaborated term)

  `fun x y => @eqb bool Eq_bool x y`

- post-pre-printing

  `Box(Box(Box(...)))`

- post-pretty-printing

  `λx y. x = y`

The fully-elaborated term should have uniquely-indexable subterms. Something like:

```
(* 1*) Fun("x",
(* 2*) Fun("y",
(* 3*) App
(* 4*)   (App
(* 5*)     (App
(* 6*)       (App
(* 7*)         eqb
(* 8*)         bool
             )
(* 9*)       Eq_bool)
(*10*)     x)
(*11*)   y
       ))
```

Other terms must understand which parts of those relate to each parts of them, and which one is their root, for instance, after pretty-printing:

```
λx λy. x = y
^^^^^^^^^^^^   1  (owns 1)
   ^^^^^^^^^   2  (owns 2)
       ^^^^^   3  (owns 3, 4, 5, 6, 7, 8, 9)
       ^       10 (owns 10)
           ^   11 (owns 11)
```

Every time a term is handed to the user, it should be annotated with those, so that the user may refer to any subterm of any term uniquely back at the system.

Elaboration: introspectable? programmable?
------------------------------------------

One might like to step through each phase of elaboration, or, at an even fine-grained level.

Steppable tactics
-----------------

Robust tactics by default
-------------------------

- One should not be able to use automatically-introduced names

Dependency tracking
-------------------

- Terms should know what other terms they depend on

- Terms should know what terms have dependended on them last!? (for refactoring purposes)

Multi-tier tactic language
--------------------------

- Tactics are either atomic, or built by composing atomic tactics.

- When a complex tactic runs, the script may only contain the tactic call, but the system should know:
  
  - what concrete atomic tactics were called
  
  - what variables were referenced or dependended upon
  
  - what subgoals were solved by what atomic tactics

Statelessness
-------------

There should not be a "current" state of the system.
Each operation should consume a "current state" and optionally produce a "new state".
