{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interop.Scala.Nothing
  (Nothing(..))
where

import GHC.Base
import GHC.Show
import Java

data Nothing = Nothing @scala.Nothing$
  deriving (Class, Show, Eq)

instance {-# OVERLAPPING #-} (Class a) => Extends Nothing a where
  superCast _ = error "Nothing"
