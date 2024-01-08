module Examples.Monoids where

type EList a = [a] -> [a] 

rep :: [a] -> EList a
rep xs = \ys -> xs ++ ys

abs :: EList a -> [a]
abs xs = xs []
