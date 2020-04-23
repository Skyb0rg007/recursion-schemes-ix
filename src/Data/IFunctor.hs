{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor
    ( IFunctor (..)
    -- * Re-exports
    , module Data.IFunction
    ) where

import           Data.Functor.Product (Product (Pair))
import           Data.Functor.Sum     (Sum (InL, InR))
import           Data.IFunction       (type (~~>))

-- | Functor in the category of dependent types
class IFunctor f where
    imap :: (a ~~> b) -> (f a ~~> f b)

instance IFunctor (Sum a) where
    imap _ (InL x) = InL x
    imap f (InR x) = InR (f x)

instance IFunctor (Product a) where
    imap f (Pair a b) = Pair a (f b)

