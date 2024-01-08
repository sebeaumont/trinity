{-# LANGUAGE NoImplicitPrelude #-}
module Examples.Category.Hask where

import qualified GHC.Base as Base (id, (.))
import Examples.Category 

instance Category (->) where
  id = Base.id
  (.) = (Base..)
