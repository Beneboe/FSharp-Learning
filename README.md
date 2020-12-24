# FSharp-Learning

## BooleanLogic

This project contains a function `DNF` that takes a boolean expression and transforms it into the disjunctive normal form (DNF). The disjunctive normal form is an Or consisting of Ands. For example:

```
(A ∧ B) ∨ (C ∧ D)
```

The following can be converted to DNF by using the distributive law:

```
(A ∨ B) ∧ (C ∨ D) = (A ∧ (C ∨ D)) ∨ (B ∧ (C ∨ D)) = (A ∧ C) ∨ (A ∧ D) ∨ (B ∧ C) ∨ (B ∧ D)
```