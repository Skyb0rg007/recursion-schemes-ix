{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IMonad
    ( IMonad (..)
    -- * Re-exports
    , module Data.IFunctor
    ) where

import           Data.Functor.Sum (Sum (InL, InR))
import           Data.IFunctor    (IFunctor (imap), type (~~>))

class IFunctor f => IMonad f where
    ipure :: a ~~> f a
    ijoin :: f (f a) ~~> f a
    ijoin = ibind id
    ibind :: (a ~~> f b) -> (f a ~~> f b)
    ibind f = ijoin . imap f
    {-# MINIMAL ipure, (ijoin | ibind) #-}

instance IMonad (Sum a) where
    ipure = InR
    ijoin (InL x) = InL x
    ijoin (InR x) = x

