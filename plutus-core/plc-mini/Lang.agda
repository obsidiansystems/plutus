module Lang where

open import Haskell.Prelude hiding (e)

data Exp  : Set where
  Val : Nat → Exp
  Add : Exp → Exp → Exp

variable
  e e1 e2 e3 e4 : Exp

instance
  postulate
    expShow : Show Exp

{-# COMPILE AGDA2HS Exp deriving Show #-}

eval : Exp -> Nat
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2

{-# COMPILE AGDA2HS eval #-}

-- Equational Specification

data _≃_ : Exp → Exp → Set where
  reflE : e ≃ e
  symE  : e1 ≃ e2 → e2 ≃ e1
  transE : e1 ≃ e2 → e2 ≃ e3 → e1 ≃ e3

  congE : e1 ≃ e2 → e3 ≃ e4 → Add e1 e3 ≃ Add e2 e4

  lunit : Add (Val 0) e ≃ e
  runit : Add e (Val 0) ≃ e
  assoc : Add (Add e1 e2) e3 ≃ Add e1 (Add e2 e3)

  β     : ∀ n n' → Add (Val n) (Val n') ≃ Val (n + n')

open import Relation.Binary.PropositionalEquality
open import Data.Nat.Properties

-- Proof that eval implmenents the spec

-- soundness
eval-correct1 : e1 ≃ e2 → eval e1 ≡ eval e2
eval-correct1 reflE = refl
eval-correct1 (symE p) = sym (eval-correct1 p)
eval-correct1 (transE p q) = trans (eval-correct1 p) (eval-correct1 q)
eval-correct1 (congE p q) = cong₂ _+_ (eval-correct1 p) (eval-correct1 q)
eval-correct1 lunit = refl
eval-correct1 runit = +-identityʳ _
eval-correct1 (assoc {e1 = e1}) = +-assoc (eval e1) _ _
eval-correct1 (β n n') = refl

--completeness
eval-correct2 : ∀ e → e ≃ Val (eval e)
eval-correct2 (Val n)     = reflE
eval-correct2 (Add e1 e2) = transE
  (congE (eval-correct2 e1) (eval-correct2 e2))
  (β (eval e1) (eval e2))

-- stability
eval-correct3 : ∀ n → eval (Val n) ≡ n
eval-correct3 n = refl
