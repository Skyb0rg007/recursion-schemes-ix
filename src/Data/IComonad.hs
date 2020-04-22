{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IComonad
    ( IComonad (..)
    -- * Re-exports
    , module Data.IFunctor
    ) where

import           Data.Functor.Product (Product (Pair))
import           Data.IFunctor        (IFunctor (imap), type (~~>))

-- | Comonoid in the category of dependent endofunctors
class IFunctor f => IComonad f where
    iextract :: f a ~~> a
    iduplicate :: f a ~~> f (f a)
    iduplicate = iextend id
    iextend :: (f a ~~> b) -> (f a ~~> f b)
    iextend f = imap f . iduplicate
    {-# MINIMAL iextract, (iduplicate | iextend) #-}

instance IComonad (Product a) where
    iextract (Pair _ b) = b
    iduplicate (Pair a b) = Pair a (Pair a b)

