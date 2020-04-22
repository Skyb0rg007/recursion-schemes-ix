{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunctor.Foldable
    ( module Data.IFunctor.Foldable
    , module X
    ) where

-- base
import           Data.Functor.Classes
import           Data.Functor.Const    as X (Const (Const, getConst))
import           Data.Functor.Identity as X (Identity (Identity, runIdentity))
import           Data.Functor.Product  as X (Product (Pair))
import           Data.Functor.Sum      as X (Sum (InL, InR))
import           Data.Kind             as X (Type)
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic, Generic1)
import           Text.Read             (Read (readPrec))
import Data.Data (Data)
-- singletons
import           Data.Singletons       as X (SingI (sing), withSingI)

infixr 5 ::<
infixr 4 ~~>

-- | Fixpoint type
newtype IFix f ix = IFix { unIFix :: f (IFix f) ix }
    deriving (Typeable, Generic, Generic1)

instance (forall a. Show1 a => Show1 (f a)) => Show1 (IFix f) where
    liftShowsPrec sp sl p (IFix x) =
         showsUnaryWith (liftShowsPrec sp sl) "IFix" p x
instance (forall a. Read1 a => Read1 (f a)) => Read1 (IFix f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "IFix" IFix
instance (forall a. Eq1 a => Eq1 (f a)) => Eq1 (IFix f) where
    liftEq eq (IFix a) (IFix b) = liftEq eq a b
instance (forall a. Eq1 a => Eq1 (f a), forall a. Ord1 a => Ord1 (f a)) => Ord1 (IFix f) where
    liftCompare comp (IFix a) (IFix b) = liftCompare comp a b
instance (forall a. Show1 a => Show1 (f a), Show ix) => Show (IFix f ix) where
    showsPrec = showsPrec1
instance (forall a. Read1 a => Read1 (f a), Read ix) => Read (IFix f ix) where
    readPrec = readPrec1
instance (forall a. Eq1 a => Eq1 (f a), Eq ix) => Eq (IFix f ix) where
    (==) = eq1
instance (forall a. Eq1 a => Eq1 (f a), forall a. Ord1 a => Ord1 (f a), Ord ix) => Ord (IFix f ix) where
    compare = compare1

-- | Indexed function type
type a ~~> b = forall ix. SingI ix => a ix -> b ix

-- | Functor in the category of dependent types
class IFunctor f where
    imap :: (a ~~> b) -> (f a ~~> f b)

-- | Monoids in the category of dependent endofunctors
class IFunctor f => IMonad f where
    ipure :: a ~~> f a
    ijoin :: f (f a) ~~> f a
    ijoin = ibind id
    ibind :: (a ~~> f b) -> (f a ~~> f b)
    ibind f = ijoin . imap f
    {-# MINIMAL ipure, (ijoin | ibind) #-}

-- | Comonoid in the category of dependent endofunctors
class IFunctor f => IComonad f where
    iextract :: f a ~~> a
    iduplicate :: f a ~~> f (f a)
    iduplicate = iextend id
    iextend :: (f a ~~> b) -> (f a ~~> f b)
    iextend f = imap f . iduplicate
    {-# MINIMAL iextract, (iduplicate | iextend) #-}

-- * Morphisms

-- | Catamorphism, ie. fold
cata :: IFunctor f
     => (f a ~~> a)
     -> (IFix f ~~> a)
cata f = gfold distCata (f . imap runIdentity1)

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
ana g = gunfold distAna (imap Identity1 . g)

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
hylo f g = ghylo distCata distAna (f . imap runIdentity1) (imap Identity1 . g)

-- | Dynamorphism
dyna :: IFunctor f
     => (f (ICofree f b) ~~> b)
     -> (a ~~> f a)
     -> (a ~~> b)
dyna f g = ghylo distHisto distAna f (imap Identity1 . g)

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

-- * Zygohistomorphic prepromorphism

-- | Zygohistomorphic prepromorphism
zygoHistoPrepro :: (IFunctor f)
                => (f (Const b) ~~> (Const b))
                -> (forall c. f c ~~> f c)
                -> (f (IEnvT b (ICofree f) a) ~~> a)
                -> (IFix f ~~> a)
zygoHistoPrepro f g t = gprepro (distZygoT f distHisto) g t

-- * Distribution laws

-- | Type of distribution laws
type DistLaw f g = forall a. f (g a) ~~> g (f a)

-- * Constructive distribution laws

distCata :: IFunctor f => DistLaw f Identity1
distCata = Identity1 . imap runIdentity1

distPara :: IFunctor f => DistLaw f (Product (IFix f))
distPara = distZygo IFix

distZygo :: IFunctor f => (f b ~~> b) -> DistLaw f (Product b)
distZygo g m = Pair (g (imap (\(Pair a _) -> a) m)) (imap (\(Pair _ b) -> b) m)

distHisto :: IFunctor f => DistLaw f (ICofree f)
distHisto x = imap (\(a ::< _) -> a) x ::< imap (\(_ ::< x) -> distHisto x) x

-- * Destructive distribution laws

distAna :: IFunctor f => DistLaw Identity1 f
distAna = imap Identity1 . runIdentity1

distApo :: IFunctor f => DistLaw (Sum (IFix f)) f
distApo = distGApo unIFix

distGApo :: IFunctor f => (b ~~> f b) -> DistLaw (Sum b) f
distGApo f (InL x) = imap InL $ f x
distGApo _ (InR x) = imap InR x

distFutu :: IFunctor f => DistLaw (IFree f) f
distFutu (IPure x) = imap IPure x
distFutu (IFree x) = imap (IFree . distFutu) x

-- * Misc

distZygoT :: (IFunctor f, IComonad w)
          => (f (Const b) ~~> (Const b))
          -> (forall c. f (w c) ~~> w (f c))
          -> (f (IEnvT b w a) ~~> IEnvT b w (f a))
distZygoT g k fe = IEnvT (getConst $ g (imap getEnv fe)) (k (imap lower fe))
    where
        lower (IEnvT _ x) = x
        getEnv :: IEnvT e w a ix -> Const e ix
        getEnv (IEnvT x _) = Const x

-- * Generic combinators

-- | Generic fold
gfold :: forall f w a. (IFunctor f, IComonad w)
      => DistLaw f w
      -> (f (w a) ~~> a)
      -> (IFix f ~~> a)
gfold k g = g . iextract . c
    where
        c :: IFix f ~~> w (f (w a))
        c = k . imap (iduplicate . imap g . c) . unIFix

-- | Generic unfold
gunfold :: forall f m a. (IFunctor f, IMonad m)
        => DistLaw m f
        -> (a ~~> f (m a))
        -> (a ~~> IFix f)
gunfold k f = a . ipure . f
    where
        a :: m (f (m a)) ~~> IFix f
        a = IFix . imap (a . imap f . ijoin) . k

-- | Generic hylomorphism
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

-- * Data Structures

-- | Identity IFunctor
data Identity1 f ix = Identity1 { runIdentity1 :: f ix }
    deriving (Typeable, Data, Generic, Generic1)

instance Show1 f => Show1 (Identity1 f) where
    liftShowsPrec sp sl p (Identity1 x) =
        showsUnaryWith (liftShowsPrec sp sl) "Identity1" p x
instance Read1 f => Read1 (Identity1 f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Identity1" Identity1
instance Eq1 f => Eq1 (Identity1 f) where
    liftEq eq (Identity1 x) (Identity1 y) = liftEq eq x y
instance Ord1 f => Ord1 (Identity1 f) where
    liftCompare comp (Identity1 x) (Identity1 y) = liftCompare comp x y
instance (Show1 f, Show ix) => Show (Identity1 f ix) where
    showsPrec = showsPrec1
instance (Read1 f, Read ix) => Read (Identity1 f ix) where
    readsPrec = readsPrec1
instance (Eq1 f, Eq ix) => Eq (Identity1 f ix) where
    (==) = eq1
instance (Ord1 f, Ord ix) => Ord (Identity1 f ix) where
    compare = compare1
instance IFunctor Identity1 where
    imap f (Identity1 x) = Identity1 (f x)
instance IComonad Identity1 where
    iextract (Identity1 x) = x
    iduplicate = Identity1
instance IMonad Identity1 where
    ipure = Identity1
    ijoin (Identity1 x) = x

-- | Cofree IComonad
data ICofree f a ix = a ix ::< f (ICofree f a) ix
    deriving (Typeable, Generic, Generic1)

instance (forall a. Show1 a => Show1 (f a), Show1 a) => Show1 (ICofree f a) where
    liftShowsPrec sp sl p (a ::< x) =
        showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "::<" p a x
instance (forall a. Read1 a => Read1 (f a), Read1 a) => Read1 (ICofree f a) where
    liftReadsPrec rp rl = readsData $
        readsBinaryWith (liftReadsPrec rp rl) (liftReadsPrec rp rl) "::<" (::<)
instance (forall a. Eq1 a => Eq1 (f a), Eq1 a) => Eq1 (ICofree f a) where
    liftEq eq (a ::< x) (b ::< y) = liftEq eq a b && liftEq eq x y
instance (forall a. Eq1 a => Eq1 (f a), forall a. Ord1 a => Ord1 (f a), Ord1 a) => Ord1 (ICofree f a) where
    liftCompare comp (a ::< x) (b ::< y) = liftCompare comp a b <> liftCompare comp x y
instance (forall a. Show1 a => Show1 (f a), Show1 a, Show ix) => Show (ICofree f a ix) where
    showsPrec = showsPrec1
instance (forall a. Read1 a => Read1 (f a), Read1 a, Read ix) => Read (ICofree f a ix) where
    readsPrec = readsPrec1
instance (forall a. Eq1 a => Eq1 (f a), Eq1 a, Eq ix) => Eq (ICofree f a ix) where
    (==) = eq1
instance (forall a. Eq1 a => Eq1 (f a), forall a. Ord1 a => Ord1 (f a), Ord1 a, Ord ix) => Ord (ICofree f a ix) where
    compare = compare1
instance IFunctor f => IFunctor (ICofree f) where
    imap f (a ::< x) = f a ::< imap (imap f) x
instance IFunctor f => IComonad (ICofree f) where
    iextract (a ::< _) = a
    iduplicate (a ::< x) = (a ::< x) ::< imap iduplicate x
    iextend f (a ::< x) = f (a ::< x) ::< imap (iextend f) x

-- | Free IMonad
data IFree f a ix
    = IPure (a ix)
    | IFree (f (IFree f a) ix)
    deriving (Typeable, Generic, Generic1)

instance (forall a. Show1 a => Show1 (f a), Show1 a) => Show1 (IFree f a) where
    liftShowsPrec sp sl p (IPure x) =
        showsUnaryWith (liftShowsPrec sp sl) "IPure" p x
    liftShowsPrec sp sl p (IFree x) =
        showsUnaryWith (liftShowsPrec sp sl) "IFree" p x
instance (forall a. Read1 a => Read1 (f a), Read1 a) => Read1 (IFree f a) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "IFree" IFree
        <>
        readsUnaryWith (liftReadsPrec rp rl) "IPure" IPure
instance (forall a. Eq1 a => Eq1 (f a), Eq1 a) => Eq1 (IFree f a) where
    liftEq eq (IPure x) (IPure y) = liftEq eq x y
    liftEq eq (IFree x) (IFree y) = liftEq eq x y
    liftEq _ _ _ = False
instance (forall a. Eq1 a => Eq1 (f a), forall a. Ord1 a => Ord1 (f a), Ord1 a) => Ord1 (IFree f a) where
    liftCompare comp (IPure x) (IPure y) = liftCompare comp x y
    liftCompare comp (IFree x) (IFree y) = liftCompare comp x y
    liftCompare _ (IPure _) (IFree _) = LT
    liftCompare _ (IFree _) (IPure _) = GT
instance (forall a. Show1 a => Show1 (f a), Show1 a, Show ix) => Show (IFree f a ix) where
    showsPrec = showsPrec1
instance (forall a. Read1 a => Read1 (f a), Read1 a, Read ix) => Read (IFree f a ix) where
    readsPrec = readsPrec1
instance (forall a. Eq1 a => Eq1 (f a), Eq1 a, Eq ix) => Eq (IFree f a ix) where
    (==) = eq1
instance (forall a. Eq1 a => Eq1 (f a), forall a. Ord1 a => Ord1 (f a), Ord1 a, Ord ix) => Ord (IFree f a ix) where
    compare = compare1
instance IFunctor f => IFunctor (IFree f) where
    imap f (IPure x) = IPure (f x)
    imap f (IFree x) = IFree (imap (imap f) x)
instance IFunctor f => IMonad (IFree f) where
    ipure = IPure
    ijoin (IPure x) = x
    ijoin (IFree x) = IFree $ imap ijoin x
    ibind f (IPure x) = f x
    ibind f (IFree x) = IFree $ imap (ibind f) x

-- -- | Church-encoded free IMonad
-- newtype F f a ix = F { runF :: forall r. (a ~~> r) -> (f r ~~> r) -> r ix }
    -- deriving (Typeable)

-- instance IFunctor (F f) where
    -- imap f (F g) = F $ \kp -> g (kp . f)
-- instance IMonad (F f) where
    -- ipure x = F $ \kp _ -> kp x
    -- ibind f (F m) = F $ \kp kf -> m (\a -> runF (f a) kp kf) kf

-- fromF :: F f a ~~> IFree f a
-- fromF (F m) = m IPure IFree

-- toF :: forall f a. IFunctor f => IFree f a ~~> F f a
-- toF xs = F $ \kp kf -> go kp kf xs
    -- where
        -- go :: forall r. (a ~~> r) -> (f r ~~> r) -> (IFree f a ~~> r)
        -- go kp _ (IPure x) = kp x
        -- go kp kf (IFree x) = kf (imap (go kp kf) x)

-- | Environment IComonad
data IEnvT e w a ix = IEnvT e (w a ix)
    deriving (Data, Typeable, Generic, Generic1)

instance IFunctor w => IFunctor (IEnvT e w) where
    imap f (IEnvT e x) = IEnvT e (imap f x)
instance (IComonad w) => IComonad (IEnvT e w) where
    iextract (IEnvT _ x) = iextract x
    iduplicate (IEnvT e x) = IEnvT e (iextend (IEnvT e) x)

-- | Composition of IFunctors
data ICompose f g a ix = ICompose { getCompose :: f (g a) ix }
    deriving (Data, Typeable, Generic, Generic1)

instance (IFunctor f, IFunctor g) => IFunctor (ICompose f g) where
    imap f (ICompose x) = ICompose $ imap (imap f) x

-- Other instances

instance IFunctor (Sum a) where
    imap f = \case
        InL x -> InL x
        InR x -> InR (f x)
instance IMonad (Sum a) where
    ipure = InR
    ijoin (InL x) = InL x
    ijoin (InR x) = x
    ibind _ (InL x) = InL x
    ibind f (InR x) = f x

instance IFunctor (Product a) where
    imap f (Pair a b) = Pair a (f b)
instance IComonad (Product a) where
    iextract (Pair _ b) = b
    iduplicate (Pair a b) = Pair a (Pair a b)
    iextend f (Pair a b) = Pair a $ f (Pair a b)
