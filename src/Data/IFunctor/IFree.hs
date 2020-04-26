{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.IFree
    ( IFree (..)
    ) where

import           Data.IFunctor         (IFunctor (..))
import           Data.IFunctor.Classes
import           Data.IMonad           (IMonad (..))
import           Data.ITraversable     (ITraversable (..))
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic, Generic1)
import           Singlethongs          (SingI)
import           Text.Read

-- | Free IMonad
data IFree f a ix = IPure (a ix)
    | IFree (f (IFree f a) ix)
    deriving (Typeable, Generic, Generic1)

instance IFunctor f => IFunctor (IFree f) where
    imap f (IPure x) = IPure (f x)
    imap f (IFree x) = IFree (imap (imap f) x)

instance ITraversable f => ITraversable (IFree f) where
    itraverse f (IPure x) = IPure <$> f x
    itraverse f (IFree x) = IFree <$> itraverse (itraverse f) x

instance IFunctor f => IMonad (IFree f) where
    ipure = IPure
    ijoin (IPure x) = x
    ijoin (IFree x) = IFree $ imap ijoin x
    ibind f (IPure x) = f x
    ibind f (IFree x) = IFree $ imap (ibind f) x

instance IShow f => IShow (IFree f) where
    ishowsPrec sp p (IPure x) = showParen (p > 10) $
        showString "IPure " . sp 11 x
    ishowsPrec sp p (IFree x) = showParen (p > 10) $
        showString "IFree " . (ishowsPrec (ishowsPrec sp)) 11 x

instance IRead f => IRead (IFree f) where
    ireadPrec rp = parens $
        (prec 10 $ do
            Ident "IPure" <- lexP
            x <- step rp
            pure $ IPure x
        )
        +++
        (prec 10 $ do
            Ident "IFree" <- lexP
            x <- step (ireadPrec (ireadPrec rp))
            pure $ IFree x
        )

instance IEq f => IEq (IFree f) where
    ieq eq (IPure x) (IPure y) = eq x y
    ieq eq (IFree x) (IFree y) = ieq (ieq eq) x y
    ieq _ _ _                  = False

instance IOrd f => IOrd (IFree f) where
    icompare comp (IPure x) (IPure y) = comp x y
    icompare comp (IFree x) (IFree y) = icompare (icompare comp) x y
    icompare _ (IPure _) (IFree _)    = LT
    icompare _ (IFree _) (IPure _)    = GT

instance (IShow f, IShow2 a, SingI ix) => Show (IFree f a ix) where
    showsPrec = ishowsPrec1

instance (IRead f, IRead2 a, SingI ix) => Read (IFree f a ix) where
    readPrec = ireadPrec1
    readListPrec = readListPrecDefault

instance (IEq f, IEq2 a, SingI ix) => Eq (IFree f a ix) where
    (==) = ieq1

instance (IOrd f, IOrd2 a, SingI ix) => Ord (IFree f a ix) where
    compare = icompare1


