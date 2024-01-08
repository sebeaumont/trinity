{-# LANGUAGE LambdaCase #-}
module TT.Eval where

import Prelude hiding (fst, snd)
import qualified TT.Semantics as V
import qualified TT.Syntax as S

-- | project value from environment
proj :: V.Env -> S.Ix -> V.Value
proj env ix =
  case env of
    V.Empty -> undefined
    V.Extend env' v ->
      if ix == 0 then v
      else proj env' (ix - 1)

      
eval :: V.Env -> S.Term -> V.Value
eval env term =
  case term of
    S.Var ix ->
      proj env ix
    S.Pi base fam ->
      let vbase = eval env base
          cfam = V.Closure fam env
      in V.Pi vbase cfam
    S.Sg base fam ->
      let vbase = eval env base
          cfam = V.Closure fam env
      in V.Sg vbase cfam
    S.Lam binder ->
      V.Lam $ V.Closure binder env
    S.App fn arg ->
      let vfn = eval env fn
          varg = eval env arg
      in app vfn varg
    S.Pair term1 term2 ->
      let v1 = eval env term1
          v2 = eval env term2
      in V.Pair v1 v2
    S.Fst pair ->
      fst (eval env pair)
    S.Snd pair ->
      snd (eval env pair)

    -- TODO more here
    _ -> undefined

app :: V.Value -> V.Value -> V.Value
app f x = case f of
  V.Lam (V.Closure (S.Binder term) env) ->
    let env' = V.Extend env x
    in eval env' term
  V.Stuk stuck typ ->
    case typ of
      V.Pi base (V.Closure (S.Binder fam) env) ->
        let stuck' = V.App { V.fn = stuck, V.arg = x, V.base = base }
            fiber = eval (V.Extend env x) fam
        in V.Stuk stuck' fiber
  _ -> undefined

fst :: V.Value -> V.Value
fst = \case
  V.Pair v _ -> v
  V.Stuk vs typ ->
    case typ of
      V.Sg base _ ->
        V.Stuk { V.value = V.Fst vs,  V.typ = base }
  _ -> undefined

snd :: V.Value -> V.Value
snd pair = case pair of
  V.Pair v _ -> v
  V.Stuk vs typ ->
    case typ of
      V.Sg _base (V.Closure (S.Binder fam) env) ->
        let u = fst pair
            fiber = eval (V.Extend env u) fam
        in V.Stuk { V.value = V.Snd vs,  V.typ = fiber }
      _ -> undefined
  _ -> undefined
