module TT.Equality where

import qualified TT.Semantics as V
import qualified TT.Syntax as S
import qualified TT.Eval as E

-- | Definitional equality for types 
equalTypes :: Int -> V.Value -> V.Value -> Bool
equalTypes len t0 t1 = case (t0, t1) of
  (V.Pi base0 (V.Closure (S.Binder fam0) env0),
   V.Pi base1 (V.Closure (S.Binder fam1) env1)) ->
    equalTypes len base0 base1 &&
    let var = V.Stuk { V.value = V.Var (V.Level len), V.typ = base0 }
        fiber0 = E.eval (V.Extend env0 var) fam0
        fiber1 = E.eval (V.Extend env1 var) fam1
    in equalTypes (len + 1) fiber0 fiber1

  (V.Pi _ _, _) -> False

  (V.Sg base0 (V.Closure (S.Binder fam0) env0),
   V.Sg base1 (V.Closure (S.Binder fam1) env1)) ->
    equalTypes len base0 base1 &&
    let var = V.Stuk { V.value = V.Var (V.Level len), V.typ = base0 }
        fiber0 = E.eval (V.Extend env0 var) fam0
        fiber1 = E.eval (V.Extend env1 var) fam1
    in equalTypes (len + 1) fiber0 fiber1

  (V.Sg _ _, _) -> False
  
  (V.Bool, V.Bool) -> True

  -- TODO: what else?
  (_, _) -> False

-- | Compare two values at a type
equal :: Int -> V.Value -> V.Value -> V.Value -> Bool  
equal len ty t0 t1 = case ty of
  
  V.Pi base (V.Closure (S.Binder fam) env) ->
    let var = V.Stuk { V.value = V.Var (V.Level len), V.typ = base }
        res0 = E.app t0 var
        res1 = E.app t1 var
        fiber = E.eval (V.Extend env var) fam
    in equal (len + 1) fiber res0 res1

  V.Sg base (V.Closure (S.Binder fam) env) ->
    let fst0 = E.fst t0
        fst1 = E.fst t1
    in equal len base fst0 fst1 &&
    let snd0 = E.snd t0
        snd1 = E.snd t1
        fiber = E.eval (V.Extend env fst0) fam
    in equal len fiber snd0 snd1 
       
  -- | Following have no η laws so we ignore types and look at values...
  _ -> case (t0, t1) of
    
    (V.True, V.True) -> True
    (V.True, _) -> False
    (V.False, V.False) -> True
    (V.False, _) -> False

    (V.Stuk sv0 typ0, V.Stuk sv1 typ1) ->
      equalTypes len typ0 typ1 &&
      equalStuck len sv0 sv1
      
    _ -> False

-- | Compare stuck values
equalStuck :: Int -> V.Stuck -> V.Stuck -> Bool
equalStuck len st0 st1 =
  case (st0, st1) of
    (V.Var v0, V.Var v1) -> v0 == v1
    (V.Fst st0', V.Fst st1') -> equalStuck len st0' st1'
    (V.Snd st0', V.Snd st1') -> equalStuck len st0' st1'
    (V.App f0 arg0 base0, V.App f1 arg1 base1) ->
      equalStuck len f0 f1 &&
      equalTypes len base0 base1 &&
      equal len base0 arg0 arg1 
    _ -> False
    
