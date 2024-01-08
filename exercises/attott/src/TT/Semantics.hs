module TT.Semantics where

import qualified TT.Syntax as Syntax

-- | Variable envionment 
data Env
  = Empty
  | Extend Env Value

-- | Type family 
data Closure
  = Closure { binder :: Syntax.Binder Syntax.Term, env :: Env }

-- | Constructor forms so no reductions
data Value
  = Pi Value Closure
  | Sg Value Closure
  | Lam Closure
  | Pair Value Value
  | Bool
  | True
  | False
  | Stuk { value :: Stuck, typ :: Value }
  
-- | Left to right index
newtype Var = Level Int
  deriving Eq

data Stuck
  = Var Var
  | Fst Stuck
  | Snd Stuck
  | App { fn :: Stuck, arg :: Value, base :: Value }
  | BoolInd { motive :: Closure , fcase :: Value , tcase :: Value , scrut :: Stuck }

