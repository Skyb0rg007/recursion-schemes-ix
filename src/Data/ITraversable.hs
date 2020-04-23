{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.ITraversable
    ( ITraversable (..)
    , imapDefault
    -- * Re-exports
    , module Data.IFunctor
    ) where

import           Data.Functor.Identity (Identity (Identity, runIdentity))
import           Data.Functor.Product  (Product (Pair))
import           Data.Functor.Sum      (Sum (InL, InR))
import           Data.IFunctor
import           Data.Singletons       (SingI)

class IFunctor f => ITraversable f where
    itraverse :: (Applicative m, SingI ix)
              => (forall ix. SingI ix => a ix -> m (b ix))
              -> f a ix
              -> m (f b ix)

-- | Default 'imap' for deriving 'IFunctor'
imapDefault :: ITraversable f
            => (a ~~> b)
            -> (f a ~~> f b)
imapDefault f = runIdentity . itraverse (Identity . f)

instance ITraversable (Sum a) where
    itraverse _ (InL x) = pure $ InL x
    itraverse f (InR x) = InR <$> f x

instance ITraversable (Product a) where
    itraverse f (Pair a b) = Pair a <$> f b

