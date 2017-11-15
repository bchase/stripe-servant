{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Util
  ( applyTo
  ) where

import Servant.API


-- applyTo
--
-- https://groups.google.com/forum/#!topic/haskell-servant/PwNpUz19QX0
-- http://lpaste.net/352272

data Functions a t t' where
  One :: (a -> b) -> Functions a (a -> b) b
  More :: Functions a t1 t1'
       -> Functions a t2 t2'
       -> Functions a (t1 :<|> t2) (t1' :<|> t2')

class Func t where
  type Arg t :: *
  type Out t :: *
  funcOf :: t -> Functions (Arg t) t (Out t)

instance Func (a -> b) where
  type Arg (a -> b) = a
  type Out (a -> b) = b
  funcOf = One

instance (Func t, Func t', Arg t ~ Arg t')
      => Func (t :<|> t') where
  type Arg (t :<|> t') = Arg t
  type Out (t :<|> t') = Out t :<|> Out t'
  funcOf (t :<|> t') = More (funcOf t) (funcOf t')

runFunctions :: Functions a t b -> a -> b
runFunctions (One f) x = f x
runFunctions (More f1 f2) x =
  runFunctions f1 x :<|> runFunctions f2 x

applyTo :: Func t => Arg t -> t -> Out t
applyTo arg t = runFunctions (funcOf t) arg
