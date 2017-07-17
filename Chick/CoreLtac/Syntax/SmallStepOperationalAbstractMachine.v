From Chick Require Import
     Goal
     LocalContext
     LocalDeclaration
     Term
.

From Chick.CoreLtac Require Import
     Results
     Syntax
.


Inductive S__v : Type :=
| Let    : forall (g : goal) (x : variable) (e : expr) (s : S__v), S__v
| App    : forall (g : goal) (es : list expr) (s : S__v), S__v
| Args   : forall (g : goal) (es : list expr) (s : S__a), S__v
| Pat    : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
             (s : S__v), S__v
| EExec1 : forall (g : goal) (s : S__v), S__v
| EExpr  : forall (g : goal) (s : S__x), S__v

with

S__a : Type :=
| Args1 : forall (g : goal) (l : lam) (s : S__v), S__a
| Args2 : forall (v : value) (s : S__a), S__a

with

S__x : Type :=
| Nil    : S__x
| Prog   : forall (g : goal) (s : S__x), S__x
| First  : forall (g : goal) (es : list expr) (s : S__x), S__x
| Semi   : forall (e : expr) (s : S__x), S__x
| BSemi  : forall (es : list expr) (s : S__x), S__x
| Seq1   : forall (gs : list goal) (es : list expr) (s : S__x), S__x
| Seq2   : forall (gs : list goal) (s : S__x), S__x
| EExec2 : forall (s : S__v), S__x

.

Fixpoint size_S__v sv :=
  match sv with
  | Let _ _ _   s => S (size_S__v s)
  | App _ _     s => S (size_S__v s)
  | Args _ _    s => S (size_S__a s)
  | Pat _ _ _ _ s => S (size_S__v s)
  | EExec1 _    s => S (size_S__v s)
  | EExpr  _    s => S (size_S__x s)
  end

with
size_S__a sa :=
  match sa with
  | Args1 _ _ s => S (size_S__v s)
  | Args2 _   s => S (size_S__a s)
  end

with
size_S__x sx :=
  match sx with
  | Nil         => 0
  | Prog _    s => S (size_S__x s)
  | First _ _ s => S (size_S__x s)
  | Semi _    s => S (size_S__x s)
  | BSemi _   s => S (size_S__x s)
  | Seq1 _ _  s => S (size_S__x s)
  | Seq2 _    s => S (size_S__x s)
  | EExec2    s => S (size_S__v s)
  end.

Inductive configuration : Type :=
| E__v    : forall (g : goal) (e : expr) (s : S__v), configuration
    (* I think there's a typo in the thesis here, is says A__v takes a RX *)
| A__v    : forall (s : S__v) (r : RV), configuration
| E__x    : forall (g : goal) (t : tactic) (s : S__x), configuration
| A__x    : forall (s : S__x) (r : RX), configuration
| E__ee   : forall (g : goal) (e : expr) (s : S__x), configuration
| E__vx   : forall (g : goal) (e : expr) (s : S__v), configuration
| E__pat  : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
                 (s : S__v), configuration
| E__args : forall (g : goal) (es : list expr) (s : S__a), configuration
| A__args : forall (s : S__a) (r : RARGS), configuration
| E__seq  : forall (g : list goal) (es : list expr) (s : S__x), configuration
.

Definition initialconfiguration (g : goal) (e : expr) :=
  E__ee g e Nil.
