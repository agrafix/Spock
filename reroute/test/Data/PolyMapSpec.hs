{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PolyMapSpec (spec) where

import Data.List (sort)
import qualified Data.PolyMap as PM
import Test.Hspec
import Text.Read (readMaybe)

intMap :: forall a. a -> PM.PolyMap Read [] a
intMap value = PM.insertWith (++) [const value :: Int -> a] PM.empty

integerMap :: forall a. a -> PM.PolyMap Read [] a
integerMap value = PM.insertWith (++) [const value :: Integer -> a] PM.empty

values :: PM.PolyMap Read [] a -> [a]
values = PM.lookupConcat (readMaybe "7") (\key functions -> map ($ key) functions)

spec :: Spec
spec =
  describe "PolyMap" $
    do
      -- Inspect one extra entry so an infinite repeated tail fails without hanging.
      it "zip produces each matching entry exactly once" $
        take 2 (values $ PM.zip (intMap (1 :: Int)) (intMap (2 :: Int)))
          `shouldBe` [(1, 2)]
      it "zipWith advances through multiple matching key types" $
        let left = intMap (1 :: Int) <> integerMap 10
            right = intMap (2 :: Int) <> integerMap 20
         in sort (take 3 $ values $ PM.zipWith (+) left right) `shouldBe` [3, 30]
      it "ap produces each matching result exactly once" $
        take 2 (values $ PM.ap (intMap (+ 1)) (intMap (2 :: Int))) `shouldBe` [3]
      it "zip omits unmatched key types" $
        values (PM.zip (intMap (1 :: Int)) (integerMap (2 :: Int))) `shouldBe` []
      it "zip handles empty maps on either side" $
        do
          values (PM.zip (intMap (1 :: Int)) (PM.empty :: PM.PolyMap Read [] Int)) `shouldBe` []
          values (PM.zip (PM.empty :: PM.PolyMap Read [] Int) (intMap (1 :: Int))) `shouldBe` []
