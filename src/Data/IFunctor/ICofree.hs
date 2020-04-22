{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.ICofree
    ( ICofree (..)
    ) where

import           Data.IComonad         (IComonad (..))
import           Data.IFunctor         (IFunctor (..))
import           Data.IFunctor.Classes
import           Data.Singletons       (SingI)
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic, Generic1)
import           Text.Read

infixr 5 ::<

-- | Cofree IComonad
data ICofree f a ix = a ix ::< f (ICofree f a) ix
    deriving (Typeable, Generic, Generic1)

instance IFunctor f => IFunctor (ICofree f) where
    imap f (a ::< x) = f a ::< imap (imap f) x

instance IFunctor f => IComonad (ICofree f) where
    iextract (a ::< _) = a
    iduplicate (a ::< x) = (a ::< x) ::< imap iduplicate x
    iextend f (a ::< x) = f (a ::< x) ::< imap (iextend f) x

instance IShow f => IShow (ICofree f) where
    ishowsPrec sp p (a ::< x) = showParen (p > 5) $
        sp 6 a . showString " ::< " . ishowsPrec (ishowsPrec sp) 6 x

instance IRead f => IRead (ICofree f) where
    ireadPrec rp = parens $ prec 5 $ do
        a <- step rp
        Symbol "::<" <- lexP
        x <- step (ireadPrec (ireadPrec rp))
        pure (a ::< x)

instance IEq f => IEq (ICofree f) where
    ieq eq (a ::< x) (a' ::< x') = eq a a' && ieq (ieq eq) x x'

instance IOrd f => IOrd (ICofree f) where
    icompare comp (a ::< x) (a' ::< x') = comp a a' <> icompare (icompare comp) x x'

instance (IShow f, SingI ix, forall ix. SingI ix => Show (a ix)) => Show (ICofree f a ix) where
    showsPrec = ishowsPrec1

instance (IRead f, SingI ix, forall ix. SingI ix => Read (a ix)) => Read (ICofree f a ix) where
    readPrec = ireadPrec1

instance (IEq f, SingI ix, forall ix. SingI ix => Eq (a ix)) => Eq (ICofree f a ix) where
    (==) = ieq1

instance (IOrd f, SingI ix, forall ix. SingI ix => Ord (a ix)) => Ord (ICofree f a ix) where
    compare = icompare1
