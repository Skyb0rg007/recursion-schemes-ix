{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.IIdentity
    ( IIdentity (IIdentity, runIIdentity)
    ) where

import           Data.Data             (Data)
import           Data.Function         (on)
import           Data.IComonad         (IComonad (..))
import           Data.IFunctor         (IFunctor (..))
import           Data.ITraversable     (ITraversable (..))
import           Data.IFunctor.Classes
import           Data.IMonad           (IMonad (..))
import           Data.Singletons       (SingI)
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic, Generic1)
import           Text.Read

data IIdentity f ix = IIdentity { runIIdentity :: f ix }
    deriving (Typeable, Data, Generic, Generic1)

instance IFunctor IIdentity where
    imap f = IIdentity . f . runIIdentity

instance IMonad IIdentity where
    ipure = IIdentity
    ijoin = runIIdentity

instance IComonad IIdentity where
    iextract = runIIdentity
    iduplicate = IIdentity

instance ITraversable IIdentity where
    itraverse f = fmap IIdentity . f . runIIdentity

instance IShow IIdentity where
    ishowsPrec sp p (IIdentity x) = showParen (p > 10) $
        showString "IIdentity " . sp p x

instance IRead IIdentity where
    ireadPrec rp = parens $ prec 10 $ do
        Ident "IIdentity" <- lexP
        x <- step rp
        pure $ IIdentity x

instance IEq IIdentity where
    ieq eq = eq `on` runIIdentity

instance IOrd IIdentity where
    icompare comp = comp `on` runIIdentity

instance (IShow2 f, SingI ix) => Show (IIdentity f ix) where
    showsPrec = ishowsPrec1

instance (IRead2 f, SingI ix) => Read (IIdentity f ix) where
    readPrec = ireadPrec1

instance (IEq2 f, SingI ix) => Eq (IIdentity f ix) where
    (==) = ieq1

instance (IOrd2 f, SingI ix) => Ord (IIdentity f ix) where
    compare = icompare1

