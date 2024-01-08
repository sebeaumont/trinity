module hello-world-proof where

open import Data.Nat using
  (ℕ; _+_; zero; suc)
open import Relation.Binary.PropositionalEquality using
  (_≡_; refl; cong)

+-assoc : Set
+-assoc = ∀ (l m n : ℕ) → l + (m + n) ≡ (l + m) + n

+-assoc-proof : ∀ (l m n : ℕ) → l + (m + n) ≡ (l + m) + n
+-assoc-proof zero m n = refl
+-assoc-proof (suc l) m n = cong suc (+-assoc-proof l m n)

