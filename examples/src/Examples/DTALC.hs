{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Examples.DTALC where
import Data.Kind (Type)

data Expr f = In (f (Expr f))

data Val e = Val Int deriving (Functor)

data Add e = Add e e deriving (Functor)

data (f :+: g) e
  = Inl (f e)
  | Inr (g e)

infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

data Where = Here | L Where | R Where

data Res = Found Where | NotFound

type family Elem (e :: Type -> Type) (f :: Type -> Type) :: Res where
  Elem e e         = Found Here
  Elem e (l :+: r) = Choose (Elem e l) (Elem e r)
  Elem e f         = NotFound

type family Choose e f :: Res where
  Choose (Found a) b = Found (L a)
  Choose a (Found b) = Found (R b)
  Choose a b         = NotFound

class Subsume (res :: Res) (f :: Type -> Type) (g :: Type -> Type) where
  inj' :: f a -> g a

instance Subsume (Found Here) f f where
  inj' = id

instance Subsume (Found p) f l => Subsume (Found (L p)) f (l :+: r) where
  inj' = Inl . inj' @(Found p)

instance Subsume (Found p) f r => Subsume (Found (R p)) f (l :+: r) where
  inj' = Inr . inj' @(Found p)

infixl 5 :<:
type f :<: g = Subsume (Elem f g) f g

infixl 5 :=:
type f :=: g = (f :<: g, g :<: f)

inj :: forall f g a . (f :<: g) => f a -> g a
inj = inj' @(Elem f g)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval e = foldExpr evalAlgebra e

injectExpr :: (g :<: f) => g (Expr f) -> Expr f
injectExpr = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = injectExpr (Val x)

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add x y = injectExpr (Add x y)

infixl 6 ⊕
(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = add x y

myexp :: Expr (Val :+: Add)
myexp = val 2000 ⊕ val 20 ⊕ val 2

ugly :: Expr (Val :+: Add)
ugly = In (Inr (Add (In (Inl (Val 2000))) (In (Inl (Val 22)))))

data Mul x = Mul x x deriving (Functor)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
mul x y = injectExpr (Mul x y)

infixl 7 ⊗
(⊗) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x ⊗ y = mul x y

expr2 :: Expr (Mul :+: Add :+: Val)
expr2 = val 4 ⊗ val 2000 ⊕ val 20 ⊕ val 2

class Render f where
  render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

instance Render Val where
  render (Val t) = show t

instance Render Add where
  render (Add x y) = "(" <> pretty x <> " + " <> pretty y <> ")"

instance Render Mul where
  render (Mul x y) = "(" <> pretty x <> " * " <> pretty y <> ")"

data Term f a where
  Pure :: a -> Term f a
  Impure :: f (Term f a) -> Term f a

instance Functor f => Functor (Term f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure (fmap f <$> t)

instance Functor f => Applicative (Term f) where
  pure = Pure
  Pure f <*> xs = f <$> xs
  Impure g <*> xs = Impure ((<*> xs) <$> g)

instance Functor f => Monad (Term f) where
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure ((>>= f) <$> t)

data Incr t = Incr Int t deriving (Functor)
data Recall t = Recall (Int -> t) deriving (Functor)

injectTerm :: (g :<: f) => g (Term f a) -> Term f a
injectTerm = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = injectTerm (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = injectTerm (Recall Pure)

tick :: Term (Recall :+: Incr) Int
tick = do
  y <- recall
  incr 1
  return y

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pur _ (Pure x) = pur x
foldTerm pur imp (Impure t) = imp (fmap (foldTerm pur imp) t)

newtype Mem = Mem Int deriving Show

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Incr where
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra

data Teletype a 
  = GetChar (Char -> a)
  | PutChar Char a
  deriving (Functor)

data FileSystem a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a
  deriving (Functor)

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

class Functor f => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra (GetChar f) = Prelude.getChar >>= f
  execAlgebra (PutChar c io) = Prelude.putChar c >> io

instance Exec FileSystem where
  execAlgebra (ReadFile name f) = Prelude.readFile name >>= f
  execAlgebra (WriteFile name s io) = Prelude.writeFile name s >> io

readFile' :: (FileSystem :<: f) => FilePath -> Term f String
readFile' name = injectTerm (ReadFile name Pure)

writeFile' :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeFile' name s = injectTerm (WriteFile name s (Pure ()))

copyFile :: FilePath -> FilePath -> Term (FileSystem) ()
copyFile from to = do
  contents <- readFile' from
  writeFile' to contents
  return ()
