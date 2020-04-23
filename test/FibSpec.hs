{-# LANGUAGE DataKinds             #-}
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
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module FibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.IFunctor.Foldable
import Data.Kind (Type)
import Data.Singletons.Prelude (STuple0 (STuple0))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "fib using 'dyna'" $ do
        it "fibs" $
            map fib [1 .. 8]
            `shouldBe`
            [1, 1, 2, 3, 5, 8, 13, 21]

fib :: Integer -> Integer
fib = getConst . fib' . Const
    where
        fib' :: Const Integer '() -> Const Integer '()
        fib' = dyna folder builder
        folder :: forall ix. SingI ix => PeanoF (ICofree PeanoF (Const Integer)) ix -> Const Integer ix
        folder OneF = 1
        folder (SuccF (_ ::< OneF)) = 1
        folder (SuccF (a ::< SuccF (b ::< _))) = a + b
        builder :: forall ix. SingI ix => Const Integer ix -> PeanoF (Const Integer) ix
        builder n =
          case sing @ix of
            STuple0 ->
              if n <= 1
                then OneF
                else SuccF (n - 1)

data PeanoF f ix where
    OneF  :: PeanoF f '()
    SuccF :: f '() -> PeanoF f '()

instance IFunctor PeanoF where
    imap _ OneF  = OneF 
    imap f (SuccF x) = SuccF (f x)


