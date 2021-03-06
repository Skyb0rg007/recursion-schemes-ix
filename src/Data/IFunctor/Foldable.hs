{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.Foldable
    (
    -- * Fixpoint for indexed types
      IFix (..)
    -- * Morphisms
    -- * Constructive morphisms
    , cata
    , prepro
    , para
    , zygo
    , histo
    -- * Destructive morphisms
    , ana
    , postpro
    , apo
    , gapo
    , futu
    -- * Combined morphisms
    , hylo
    , dyna
    , chrono
    , meta
    , elgot
    , coelgot
    -- * Monadic morphisms
    , cataM
    , paraM
    , anaM
    , apoM
    , hyloM
    , dynaM
    -- * Distribution Laws
    , DistLaw
    -- * Constructive distribution laws
    , distCata
    , distPara
    , distZygo
    , distHisto
    -- * Destructive distribution laws
    , distAna
    , distApo
    , distGApo
    , distFutu
    -- * Generalized morphisms
    , gfold
    , gunfold
    , ghylo
    , gprepro
    , gpostpro
    -- * Generalized monadic morphisms
    , gfoldM
    , gunfoldM
    , ghyloM
    -- * Re-exports
    , module Data.Functor.Const
    , module Data.Functor.Product
    , module Data.Functor.Sum
    , module Data.IComonad
    , module Data.IFunction
    , module Data.IFunctor
    , module Data.IFunctor.Classes
    , module Data.IFunctor.ICofree
    , module Data.IFunctor.IFree
    , module Data.IFunctor.IIdentity
    , module Data.IMonad
    , module Data.ITraversable
    , module Singlethongs
    ) where

import           Control.Monad           ((<=<))
import           Data.Functor.Const      (Const (..))
import           Data.Functor.Product    (Product (..))
import           Data.Functor.Sum        (Sum (..))
import           Data.IComonad           (IComonad (..))
import           Data.IFunction          (type (~~>))
import           Data.IFunctor           (IFunctor (..))
import           Data.IFunctor.Classes
import           Data.IFunctor.ICofree   (ICofree (..))
import           Data.IFunctor.IFree     (IFree (..))
import           Data.IFunctor.IIdentity (IIdentity (..))
import           Data.IMonad             (IMonad (..))
import           Data.ITraversable       (ITraversable (..), imapDefault)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic, Generic1)
import           Singlethongs            (SingI (sing))
import           Text.Read

-- | Fixpoint type
newtype IFix f ix = IFix { unIFix :: f (IFix f) ix }
    deriving (Typeable, Generic, Generic1)

instance (IShow f, SingI ix) => Show (IFix f ix) where
    showsPrec p (IFix x) = showParen (p > 10) $
        showString "IFix " .  ishowsPrec showsPrec 11 x

instance (IRead f, SingI ix) => Read (IFix f ix) where
    readPrec = parens $ prec 10 $ do
        Ident "IFix" <- lexP
        x <- step $ ireadPrec readPrec
        pure $ IFix x

instance (IEq f, SingI ix) => Eq (IFix f ix) where
    IFix x == IFix y = ieq (==) x y

instance (IOrd f, SingI ix) => Ord (IFix f ix) where
    IFix x `compare` IFix y = icompare compare x y

-- * Morphisms

-- | Catamorphism, ie. fold
cata :: IFunctor f
     => (f a ~~> a)
     -> (IFix f ~~> a)
cata f = gfold distCata (f . imap runIIdentity)

-- | Fokkinga's prepromorphism
prepro :: IFunctor f
       => (forall b. f b ~~> f b)
       -> (f a ~~> a)
       -> (IFix f ~~> a)
prepro e f = gprepro distCata e (f . imap runIIdentity)

-- | Paramorphism
para :: IFunctor f
     => (f (Product (IFix f) a) ~~> a)
     -> (IFix f ~~> a)
para = gfold distPara

-- | Zygomorphism
zygo :: IFunctor f
     => (f b ~~> b)
     -> (f (Product b a) ~~> a)
     -> (IFix f ~~> a)
zygo f = gfold (distZygo f)

-- | Histomorphism
histo :: IFunctor f
      => (f (ICofree f a) ~~> a)
      -> (IFix f ~~> a)
histo = gfold distHisto

-- | Anamorphism, ie. unfold
ana :: IFunctor f
    => (a ~~> f a)
    -> (a ~~> IFix f)
ana g = gunfold distAna (imap IIdentity . g)

-- | Fokkinga's postpromorphism
postpro :: IFunctor f
        => (forall b. f b ~~> f b)
        -> (a ~~> f a)
        -> (a ~~> IFix f)
postpro e g = gpostpro distAna e (imap IIdentity . g)

-- | Apomorphism
apo :: IFunctor f
    => (a ~~> f (Sum (IFix f) a))
    -> (a ~~> IFix f)
apo = gunfold distApo

-- | GApomorphism
gapo :: IFunctor f
     => (b ~~> f b)
     -> (a ~~> f (Sum b a))
     -> (a ~~> IFix f)
gapo f = gunfold (distGApo f)

-- | Futumorphism
futu :: IFunctor f
     => (a ~~> f (IFree f a))
     -> (a ~~> IFix f)
futu = gunfold distFutu

-- | Hylomorphism, fold then unfold
hylo :: IFunctor f
     => (f b ~~> b)
     -> (a ~~> f a)
     -> (a ~~> b)
hylo f g = ghylo distCata distAna (f . imap runIIdentity) (imap IIdentity . g)

-- | Dynamorphism
dyna :: IFunctor f
     => (f (ICofree f b) ~~> b)
     -> (a ~~> f a)
     -> (a ~~> b)
dyna f g = ghylo distHisto distAna f (imap IIdentity . g)

-- | Chronomorphism
chrono :: IFunctor f
       => (f (ICofree f b) ~~> b)
       -> (a ~~> f (IFree f a))
       -> (a ~~> b)
chrono = ghylo distHisto distFutu

-- | Metamorphism
-- TODO: ensure only 1 pass over structure
meta :: (IFunctor f, IFunctor g)
     => (f a ~~> a)
     -> (a ~~> b)
     -> (b ~~> g b)
     -> (IFix f ~~> IFix g)
meta f e g = ana g . e . cata f

-- * Elgot algebras

elgot :: forall f a b. IFunctor f
      => (f a ~~> a)
      -> (b ~~> Sum a (f b))
      -> (b ~~> a)
elgot phi psi = h
    where
        h :: b ~~> a
        h = (id `sum` (phi . imap h)) . psi
        sum :: forall a b c. (a ~~> c) -> (b ~~> c) -> (Sum a b ~~> c)
        sum f _ (InL x) = f x
        sum _ g (InR x) = g x

coelgot :: forall f a b. IFunctor f
        => (Product a (f b) ~~> b)
        -> (a ~~> f a)
        -> (a ~~> b)
coelgot phi psi = h
    where
        h :: a ~~> b
        h = phi . (id `product` (imap h . psi))
        product :: forall a b c. (a ~~> b) -> (a ~~> c) -> (a ~~> Product b c)
        product f g x = Pair (f x) (g x)


-- * Monadic morphisms

-- | Monadic catamorphism
cataM :: (ITraversable f, Monad m)
      => (forall ix. SingI ix => f a ix -> m (a ix))
      -> (forall ix. SingI ix => IFix f ix -> m (a ix))
cataM f = gfoldM distCata (f . imap runIIdentity)

-- | Monadic Paramorphism
paraM :: (ITraversable f, Monad m)
      => (forall ix. SingI ix => f (Product (IFix f) a) ix -> m (a ix))
      -> (forall ix. SingI ix => IFix f ix -> m (a ix))
paraM = gfoldM distPara

-- | Monadic anamorphism
anaM :: (ITraversable f, Monad m)
     => (forall ix. SingI ix => a ix -> m (f a ix))
     -> (forall ix. SingI ix => a ix -> m (IFix f ix))
anaM g = gunfoldM distAna (fmap (imap IIdentity) . g)

-- | Monadic apomorphism
apoM :: (ITraversable f, Monad m)
     => (forall ix. SingI ix => a ix -> m (f (Sum (IFix f) a) ix))
     -> (forall ix. SingI ix => a ix -> m (IFix f ix))
apoM = gunfoldM distApo

-- | Monadic hylomorphism
hyloM :: (ITraversable f, Monad m)
     => (forall ix. SingI ix => f b ix -> m (b ix))
     -> (forall ix. SingI ix => a ix -> m (f a ix))
     -> (forall ix. SingI ix => a ix -> m (b ix))
hyloM f g = ghyloM distCata distAna (f . imap runIIdentity) (fmap (imap IIdentity) . g)

-- | Monadic dynamorphism
dynaM :: (ITraversable f, Monad m)
      => (forall ix. SingI ix => f (ICofree f b) ix -> m (b ix))
      -> (forall ix. SingI ix => a ix -> m (f a ix))
      -> (forall ix. SingI ix => a ix -> m (b ix))
dynaM f g = ghyloM distHisto distAna f (fmap (imap IIdentity) . g)

-- * Distribution laws

-- | Type of distribution laws
-- A Constructive morphism means the second part is a 'IComonad'
-- A Destructive morphism means the first part is an 'IMonad'
type DistLaw f g = forall a. f (g a) ~~> g (f a)

-- * Constructive distribution laws

distCata :: IFunctor f => DistLaw f IIdentity
distCata = IIdentity . imap runIIdentity

distPara :: IFunctor f => DistLaw f (Product (IFix f))
distPara = distZygo IFix

distZygo :: IFunctor f => (f b ~~> b) -> DistLaw f (Product b)
distZygo g m = Pair (g (imap (\(Pair a _) -> a) m)) (imap (\(Pair _ b) -> b) m)

distHisto :: IFunctor f => DistLaw f (ICofree f)
distHisto x = imap (\(a ::< _) -> a) x ::< imap (\(_ ::< x) -> distHisto x) x

-- * Destructive distribution laws

distAna :: IFunctor f => DistLaw IIdentity f
distAna = imap IIdentity . runIIdentity

distApo :: IFunctor f => DistLaw (Sum (IFix f)) f
distApo = distGApo unIFix

distGApo :: IFunctor f => (b ~~> f b) -> DistLaw (Sum b) f
distGApo f (InL x) = imap InL $ f x
distGApo _ (InR x) = imap InR x

distFutu :: IFunctor f => DistLaw (IFree f) f
distFutu (IPure x) = imap IPure x
distFutu (IFree x) = imap (IFree . distFutu) x

-- * Generalized combinators

-- | Generalized fold
gfold :: forall f w a. (IFunctor f, IComonad w)
      => DistLaw f w
      -> (f (w a) ~~> a)
      -> (IFix f ~~> a)
gfold k g = g . iextract . c
    where
        c :: IFix f ~~> w (f (w a))
        c = k . imap (iduplicate . imap g . c) . unIFix

-- | Generalized unfold
gunfold :: forall f m a. (IFunctor f, IMonad m)
        => DistLaw m f
        -> (a ~~> f (m a))
        -> (a ~~> IFix f)
gunfold k f = a . ipure . f
    where
        a :: m (f (m a)) ~~> IFix f
        a = IFix . imap (a . imap f . ijoin) . k

-- | Generalized hylomorphism
ghylo :: forall f w m a b. (IFunctor f, IComonad w, IMonad m)
      => DistLaw f w
      -> DistLaw m f
      -> f (w b) ~~> b
      -> a ~~> f (m a)
      -> a ~~> b
ghylo w m f g = iextract . h . ipure
    where
        h :: m a ~~> w b
        h = imap f . w . imap (iduplicate . h . ijoin) . m . imap g

-- | Generalized monadic fold
gfoldM :: forall f w m a. (ITraversable f, ITraversable w, IComonad w, Monad m)
       => DistLaw f w
       -> (forall ix. SingI ix => f (w a) ix -> m (a ix))
       -> (forall ix. SingI ix => IFix f ix -> m (a ix))
gfoldM k g = g . iextract <=< c
    where
        c :: forall ix. SingI ix => IFix f ix -> m (w (f (w a)) ix)
        c = fmap k . itraverse (fmap iduplicate . itraverse g <=< c) . unIFix

-- | Generalized monadic unfold
gunfoldM :: forall f m a x. (ITraversable f, ITraversable m, IMonad m, Monad x)
         => DistLaw m f
         -> (forall ix. SingI ix => a ix -> x (f (m a) ix))
         -> (forall ix. SingI ix => a ix -> x (IFix f ix))
gunfoldM k f = a . ipure <=< f
    where
        a :: SingI ix => m (f (m a)) ix -> x (IFix f ix)
        a = fmap IFix . itraverse (a <=< itraverse f . ijoin) . k

-- | Generalized monadic hylomorphism
ghyloM :: forall f w m a b x. (ITraversable f, ITraversable w, ITraversable m, IComonad w, IMonad m, Monad x)
       => DistLaw f w
       -> DistLaw m f
       -> (forall ix. SingI ix => f (w b) ix -> x (b ix))
       -> (forall ix. SingI ix => a ix -> x (f (m a) ix))
       -> (forall ix. SingI ix => a ix -> x (b ix))
ghyloM w m f g = fmap iextract . h . ipure
    where
        h :: SingI ix => m a ix -> x (w b ix)
        h = itraverse f <=< fmap w . itraverse (fmap iduplicate . h . ijoin) . m <=< itraverse g

-- | Generalized prepromorphism
gprepro :: forall f w a. (IFunctor f, IComonad w)
        => DistLaw f w
        -> (forall c. f c ~~> f c)
        -> (f (w a) ~~> a)
        -> (IFix f ~~> a)
gprepro k e f = iextract . c
    where
        c :: IFix f ~~> w a
        c = imap f . k . imap (iduplicate . c . cata (IFix . e)) . unIFix

-- | Generalized postpromorphism
gpostpro :: forall f m a. (IFunctor f, IMonad m)
         => DistLaw m f
         -> (forall c. f c ~~> f c)
         -> (a ~~> f (m a))
         -> (a ~~> IFix f)
gpostpro k e g = a . ipure
    where
        a :: m a ~~> IFix f
        a = IFix . imap (cata (IFix . e) . a . ijoin) . k . imap g



