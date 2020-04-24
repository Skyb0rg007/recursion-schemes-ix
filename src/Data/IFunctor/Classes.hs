{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.Classes
    (
    -- * Indexed Show
      IShow (..)
    , IShow2 (..)
    , ishowsPrec1
    , ishow1
    -- * Indexed Read
    , IRead (..)
    , IRead2 (..)
    , ireadPrec1
    , ireadsPrec1
    -- * Indexed Eq
    , IEq (..)
    , IEq2 (..)
    , ieq1
    -- * Indexed Ord
    , IOrd (..)
    , IOrd2 (..)
    , icompare1
    ) where

import           Data.Functor.Product (Product (..))
import           Data.Functor.Sum     (Sum (..))
import           Singlethongs         (SingI)
import           Text.Read

class IShow f where
    ishowsPrec :: SingI ix => (forall ix. SingI ix => Int -> a ix -> ShowS) -> Int -> f a ix -> ShowS
    ishowsPrec sp _ x s = ishow sp x ++ s
    ishow :: SingI ix => (forall ix. SingI ix => Int -> a ix -> ShowS) -> f a ix -> String
    ishow sp x = ishowsPrec sp 0 x ""
    {-# MINIMAL ishowsPrec | ishow #-}

class IShow2 a where
    ishowsPrec2 :: SingI ix => Int -> a ix -> ShowS
    ishowsPrec2 _ x s = ishow2 x ++ s
    ishow2 :: SingI ix => a ix -> String
    ishow2 x = ishowsPrec2 0 x ""
    {-# MINIMAL ishowsPrec2 | ishow2 #-}

ishowsPrec1 :: (IShow f, IShow2 a, SingI ix) => Int -> f a ix -> ShowS
ishowsPrec1 = ishowsPrec ishowsPrec2

ishow1 :: (IShow f, IShow2 a, SingI ix) => f a ix -> String
ishow1 = ishow ishowsPrec2

class IRead f where
    ireadPrec :: SingI ix => (forall ix. SingI ix => ReadPrec (a ix)) -> ReadPrec (f a ix)
    ireadPrec rp = readS_to_Prec $ ireadsPrec (readPrec_to_S rp)
    ireadsPrec :: SingI ix => (forall ix. SingI ix => Int -> ReadS (a ix)) -> Int -> ReadS (f a ix)
    ireadsPrec rp = readPrec_to_S $ ireadPrec (readS_to_Prec rp)
    {-# MINIMAL ireadPrec | ireadsPrec #-}

class IRead2 a where
    ireadPrec2 :: SingI ix => ReadPrec (a ix)
    ireadPrec2 = readS_to_Prec $ ireadsPrec2
    ireadsPrec2 :: SingI ix => Int -> ReadS (a ix)
    ireadsPrec2 = readPrec_to_S $ ireadPrec2
    {-# MINIMAL ireadPrec2 | ireadsPrec2 #-}

ireadPrec1 :: (IRead f, IRead2 a, SingI ix) => ReadPrec (f a ix)
ireadPrec1 = ireadPrec ireadPrec2

ireadsPrec1 :: (IRead f, IRead2 a, SingI ix) => Int -> ReadS (f a ix)
ireadsPrec1 = ireadsPrec ireadsPrec2

class IEq f where
    ieq :: SingI ix => (forall ix. SingI ix => a ix -> a ix -> Bool) -> f a ix -> f a ix -> Bool

class IEq2 a where
    ieq2 :: SingI ix => a ix -> a ix -> Bool

ieq1 :: (IEq f, IEq2 a, SingI ix) => f a ix -> f a ix -> Bool
ieq1 = ieq ieq2

class IEq f => IOrd f where
    icompare :: SingI ix => (forall ix. SingI ix => a ix -> a ix -> Ordering) -> f a ix -> f a ix -> Ordering

class IEq2 a => IOrd2 a where
    icompare2 :: SingI ix => a ix -> a ix -> Ordering

icompare1 :: (IOrd f, IOrd2 a, SingI ix) => f a ix -> f a ix -> Ordering
icompare1 = icompare icompare2

-- * base instances

-- Sum
instance IShow2 a => IShow (Sum a) where
    ishowsPrec _ p (InL x) = showParen (p > 10) $
        showString "InL " . ishowsPrec2 11 x
    ishowsPrec sp p (InR x) = showParen (p > 10) $
        showString "InR " . sp 11 x

instance IRead2 a => IRead (Sum a) where
    ireadPrec rp = parens $
        (prec 10 $ do
            Ident "InL" <- lexP
            x <- step ireadPrec2
            pure $ InL x
        )
        +++
        (prec 10 $ do
            Ident "InR" <- lexP
            x <- step rp
            pure $ InR x
        )

instance IEq2 a => IEq (Sum a) where
    ieq _  (InL x) (InL y) = ieq2 x y
    ieq eq (InR x) (InR y) = eq x y
    ieq _ _ _              = False

instance IOrd2 a => IOrd (Sum a) where
    icompare _ (InL x) (InL y)    = icompare2 x y
    icompare comp (InR x) (InR y) = comp x y
    icompare _ (InL _) (InR _)    = LT
    icompare _ (InR _) (InL _)    = GT

-- Product
instance IShow2 a => IShow (Product a) where
    ishowsPrec sp p (Pair a b) = showParen (p > 10) $
        showString "Pair " . ishowsPrec2 11 a . showString " " . sp 11 b

instance IRead2 a => IRead (Product a) where
    ireadPrec rp = parens $ prec 10 $ do
        Ident "Pair" <- lexP
        x <- step ireadPrec2
        y <- step rp
        pure $ Pair x y

instance IEq2 a => IEq (Product a) where
    ieq eq (Pair a b) (Pair a' b') = ieq2 a a' && eq b b'

instance IOrd2 a => IOrd (Product a) where
    icompare comp (Pair a b) (Pair a' b') = icompare2 a a' <> comp b b'

