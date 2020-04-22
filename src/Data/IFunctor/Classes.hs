{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.Classes
    (
    -- * Indexed Show
      IShow (..)
    , ishowsPrec1
    , ishow1
    -- * Indexed Read
    , IRead (..)
    , ireadPrec1
    , ireadsPrec1
    -- * Indexed Eq
    , IEq (..)
    , ieq1
    -- * Indexed Ord
    , IOrd (..)
    , icompare1
    ) where

import           Data.Functor.Product (Product (..))
import           Data.Functor.Sum     (Sum (..))
import           Data.Singletons      (SingI)
import           Text.Read

class IShow f where
    ishowsPrec :: SingI ix => (forall ix. SingI ix => Int -> a ix -> ShowS) -> Int -> f a ix -> ShowS
    ishowsPrec sp _ x s = ishow sp x ++ s
    ishow :: SingI ix => (forall ix. SingI ix => Int -> a ix -> ShowS) -> f a ix -> String
    ishow sp x = ishowsPrec sp 0 x ""
    {-# MINIMAL ishowsPrec | ishow #-}

ishowsPrec1 :: (SingI ix, IShow f, forall ix. SingI ix => Show (a ix)) => Int -> f a ix -> ShowS
ishowsPrec1 = ishowsPrec showsPrec

ishow1 :: (SingI ix, IShow f, forall ix. Show (a ix)) => f a ix -> String
ishow1 = ishow showsPrec

class IRead f where
    ireadPrec :: SingI ix => (forall ix. SingI ix => ReadPrec (a ix)) -> ReadPrec (f a ix)
    ireadPrec rp = readS_to_Prec $ ireadsPrec (readPrec_to_S rp)
    ireadsPrec :: SingI ix => (forall ix. SingI ix => Int -> ReadS (a ix)) -> Int -> ReadS (f a ix)
    ireadsPrec rp = readPrec_to_S $ ireadPrec (readS_to_Prec rp)
    {-# MINIMAL ireadPrec | ireadsPrec #-}

ireadPrec1 :: (SingI ix, IRead f, forall ix. SingI ix => Read (a ix)) => ReadPrec (f a ix)
ireadPrec1 = ireadPrec readPrec

ireadsPrec1 :: (SingI ix, IRead f, forall ix. SingI ix => Read (a ix)) => Int -> ReadS (f a ix)
ireadsPrec1 = ireadsPrec readsPrec

class IEq f where
    ieq :: SingI ix => (forall ix. SingI ix => a ix -> a ix -> Bool) -> f a ix -> f a ix -> Bool

ieq1 :: (SingI ix, IEq f, forall ix. SingI ix => Eq (a ix)) => f a ix -> f a ix -> Bool
ieq1 = ieq (==)

class IEq f => IOrd f where
    icompare :: SingI ix => (forall ix. SingI ix => a ix -> a ix -> Ordering) -> f a ix -> f a ix -> Ordering

icompare1 :: (SingI ix, IOrd f, forall ix. SingI ix => Ord (a ix)) => f a ix -> f a ix -> Ordering
icompare1 = icompare compare

-- * base instances

-- Sum
instance (forall ix. SingI ix => Show (a ix)) => IShow (Sum a) where
    ishowsPrec _ p (InL x) = showParen (p > 10) $
        showString "InL " . showsPrec 11 x
    ishowsPrec sp p (InR x) = showParen (p > 10) $
        showString "InR " . sp 11 x

instance (forall ix. SingI ix => Read (a ix)) => IRead (Sum a) where
    ireadPrec rp = parens $
        (prec 10 $ do
            Ident "InL" <- lexP
            x <- step readPrec
            pure $ InL x
        )
        +++
        (prec 10 $ do
            Ident "InR" <- lexP
            x <- step rp
            pure $ InR x
        )

instance (forall ix. SingI ix => Eq (a ix)) => IEq (Sum a) where
    ieq _  (InL x) (InL y) = x == y
    ieq eq (InR x) (InR y) = eq x y
    ieq _ _ _ = False

instance (forall ix. SingI ix => Ord (a ix)) => IOrd (Sum a) where
    icompare _ (InL x) (InL y) = compare x y
    icompare comp (InR x) (InR y) = comp x y
    icompare _ (InL _) (InR _) = LT
    icompare _ (InR _) (InL _) = GT

-- Product
instance (forall ix. SingI ix => Show (a ix)) => IShow (Product a) where
    ishowsPrec sp p (Pair a b) = showParen (p > 10) $
        showString "Pair " . showsPrec 11 a . showString " " . sp 11 b

instance (forall ix. SingI ix => Read (a ix)) => IRead (Product a) where
    ireadPrec rp = parens $ prec 10 $ do
        Ident "Pair" <- lexP
        x <- step readPrec
        y <- step rp
        pure $ Pair x y

instance (forall ix. SingI ix => Eq (a ix)) => IEq (Product a) where
    ieq eq (Pair a b) (Pair a' b') = a == a' && eq b b'

instance (forall ix. SingI ix => Ord (a ix)) => IOrd (Product a) where
    icompare comp (Pair a b) (Pair a' b') = compare a a' <> comp b b'

