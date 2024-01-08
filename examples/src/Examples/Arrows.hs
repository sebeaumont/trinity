module Examples.Arrows where
import Data.Kind (Type)

class Bifunctor (f :: Type -> Type -> Type) where
  bimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'

class Profunctor (f :: Type -> Type -> Type) where
  dimap :: (a' -> a) -> (b -> b') -> f a b -> f a' b'

instance Profunctor (->) where
  dimap con pro f = pro . f . con

type End p = forall x. p x x

newtype NaturalPro f g a b = NaturalPro (f a -> g b)

instance (Functor f, Functor g) => Profunctor (NaturalPro f g) where
  dimap ba cd (NaturalPro p) =
    NaturalPro $ fmap cd . p . fmap ba

type Α f g = End (NaturalPro f g)
