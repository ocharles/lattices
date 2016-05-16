{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Divisible
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2016 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Divisible (
    Divisible(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Algebra.Lattice
import Algebra.PartialOrd

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Hashable
import GHC.Generics

--
-- Divisible
--

-- | A total order gives rise to a lattice. Join is
-- max, meet is min.
newtype Divisible a = Divisible { getDivisible :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Foldable Divisible where
  foldMap f (Divisible a) = f a

instance Traversable Divisible where
  traverse f (Divisible a) = Divisible <$> f a

instance Functor Divisible where
  fmap f (Divisible a) = Divisible (f a)

instance Applicative Divisible where
  pure = return
  (<*>) = ap

instance Monad Divisible where
  return           = Divisible
  Divisible x >>= f  = f x

instance NFData a => NFData (Divisible a) where
  rnf (Divisible a) = rnf a

instance Hashable a => Hashable (Divisible a)

instance Integral a => JoinSemiLattice (Divisible a) where
  Divisible x \/ Divisible y = Divisible (gcd x y)

instance Integral a => MeetSemiLattice (Divisible a) where
  Divisible x /\ Divisible y = Divisible (lcm x y)

instance Integral a => Lattice (Divisible a) where

instance Integral a => BoundedJoinSemiLattice (Divisible a) where
  bottom = Divisible 1 

instance (Integral a, Eq a) => PartialOrd (Divisible a) where
    leq x y = y == (x \/ y)
