{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Examples.Free where

data Free f a where
  Pure :: a -> Free f a 
  Free :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap f <$> g)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> xs = f <$> xs
  Free g <*> xs = Free ((<*> xs) <$> g)

instance Functor f => Monad (Free f) where
  return = pure
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)

join :: Functor f => Free f (Free f a) -> Free f a
join (Pure x) = x
join (Free x) = Free $ join <$> x

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure x) = return x
foldFree f (Free as) = f as >>= foldFree f

infixr 0 ~>
type f ~> g = forall x. f x -> g x

freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM _ (Pure x) = Pure x
freeM α (Free fx) = Free $ α (freeM α <$> fx)

monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx

interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp α = monad . freeM α
