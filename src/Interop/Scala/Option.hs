{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Interop.Scala.Option
  ( Option(..)
  , mapOption
  , foldOption
  , option
  , optionFromMaybe
  , optionToMaybe
  , Some(..)
  , some
  , some_
  , None(..)
  , none
  , none_
  )
where

import Data.Maybe (maybe)
import GHC.Base hiding (some)
import GHC.Show
import Java

import Interop.Scala.Function
import Interop.Scala.Nothing

data Option a = Option (@scala.Option a)
  deriving (Class, Show, Eq)

instance {-# OVERLAPPING #-} (a <: b) => Extends (Option a) (Option b)

foreign import java unsafe "map" mapOption
  :: Function1 a b -> Java (Option a) (Option b)

foreign import java unsafe "fold" foldOption
  :: (b <: Object) => Function0 b -> Function1 a b -> Java (Option a) b

option :: (a <: Object) => b -> (a -> b) -> Option a -> b
option b f o =
  maybe b (f . flip unsafePerformJavaWith someValue) (safeDowncast o)

optionFromMaybe :: (a <: Object) => Maybe a -> Option a
optionFromMaybe =
  maybe (superCast none) (superCast . some)

optionToMaybe :: (a <: Object) => Option a -> Maybe a
optionToMaybe =
  option Nothing Just

data Some a = Some (@scala.Some a)
  deriving (Class, Show, Eq)

type instance Inherits (Some a) = '[Option a]

instance {-# OVERLAPPING #-} (a <: b) => Extends (Some a) (Some b)

foreign import java unsafe "@new" some
  :: (a <: Object) => a -> Some a

foreign import java unsafe "value" someValue
  :: (a <: Object) => Java (Some a) a

some_ :: (a <: Object) => a -> Option a
some_ =
  superCast . some

data None = None @scala.None$
  deriving (Class, Show, Eq)

foreign import java unsafe "@static @field scala.None$.MODULE$" none
  :: None

instance {-# OVERLAPPING #-} (Class a) => Extends None (Option a)

none_ :: (Class a) => Option a
none_ =
  superCast none
