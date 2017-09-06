Set Implicit Arguments.

Notation "f $ x" := (f x) (right associativity, at level 180, only parsing).

Notation "f <$> x" := (option_map f x) (at level 60).
