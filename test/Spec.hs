module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified FibSpec
import qualified GRINSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "FibSpec"  FibSpec.spec
    describe "GRINSpec" GRINSpec.spec

