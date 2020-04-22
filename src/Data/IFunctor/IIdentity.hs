{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
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

instance (forall ix. SingI ix => Show (f ix), SingI ix) => Show (IIdentity f ix) where
    showsPrec = ishowsPrec1

instance (forall ix. SingI ix => Read (f ix), SingI ix) => Read (IIdentity f ix) where
    readPrec = ireadPrec1

instance (forall ix. SingI ix => Eq (f ix), SingI ix) => Eq (IIdentity f ix) where
    (==) = ieq1

instance (forall ix. SingI ix => Ord (f ix), SingI ix) => Ord (IIdentity f ix) where
    compare = icompare1

