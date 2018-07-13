{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Interop.Scala.Function
  (Function0(..), fun0, fun0Apply, Function1(..), fun1, fun1Apply)
where

import GHC.Base hiding (some)
import GHC.Show
import Java

data Function0 r = Function0 (@scala.Function0 r)
  deriving (Class, Show, Eq)

foreign import java unsafe "@wrapper apply"
  fun0 :: (r <: Object) => Java (Function0 r) r -> Function0 r

foreign import java unsafe "apply"
  fun0Apply :: (r <: Object) => Function0 r -> r

data Function1 t r = Function1 (@scala.Function1 t r)
  deriving (Class, Show, Eq)

foreign import java unsafe "@wrapper apply"
  fun1 :: (t <: Object, r <: Object) => (t -> Java (Function1 t r) r) -> Function1 t r

foreign import java unsafe "apply"
  fun1Apply :: (t <: Object, r <: Object) => Function1 t r -> t -> r
