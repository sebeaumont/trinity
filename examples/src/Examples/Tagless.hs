module Examples.Tagless where

data Exp
  = Lit Int
  | Add Exp Exp

expi :: Exp
expi = Add (Lit 23) (Lit 20)

eval :: Exp -> Int
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

render :: Exp -> String
render (Lit i) = show i
render (Add e1 e2) = "(" <> render e1 <> " + " <> render e2 <> ")"

class ExpS repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr

instance ExpS Int where
  lit n = n
  add e1 e2 = e1 + e2

eval' :: Int -> Int
eval' = id

instance ExpS String where
  lit n = show n
  add e1 e2 = "(" <> e1 <> " + " <> e2 <> ")"

pprint :: String -> String
pprint = id

class ExpMulS repr where
  mul :: repr -> repr -> repr

instance ExpMulS Int where
  mul e1 e2 = e1 * e2
instance ExpMulS String where
  mul e1 e2 = "(" <> e1 <> " * " <> e2 <> ")"
