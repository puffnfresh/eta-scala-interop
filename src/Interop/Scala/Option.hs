{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Interop.Scala.Option
  (Option(..), Some(..), some, None(..), none)
where

import GHC.Base hiding (some)
import GHC.Show
import Java

import Interop.Scala.Nothing

data Option a = Option (@scala.Option a)
  deriving (Class, Show, Eq)

instance {-# OVERLAPPING #-} (a <: b) => Extends (Option a) (Option b)

data Some a = Some (@scala.Some a)
  deriving (Class, Show, Eq)

instance {-# OVERLAPPING #-} (a <: b) => Extends (Some a) (Some b)

instance {-# OVERLAPPING #-} (a <: b) => Extends (Some a) (Option b)

foreign import java unsafe "@new" some
  :: (a <: Object) => a -> Some a

data None = None @scala.None$
  deriving (Class, Show, Eq)

foreign import java unsafe "@static @field scala.None$.MODULE$" none
  :: None

instance {-# OVERLAPPING #-} (Class a) => Extends None (Option a)
