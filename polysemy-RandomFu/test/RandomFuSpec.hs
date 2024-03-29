{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RandomFuSpec where

import           Polysemy
import           Polysemy.RandomFu

import           Test.Hspec
import           Control.Monad                 as M

import qualified Data.Random                   as R
import qualified System.Random.Mersenne.Pure64 as MR
import qualified Data.Vector as V

getRandomInts :: Member RandomFu r => Int -> Sem r [Int]
getRandomInts nDraws =
  sampleRVar $ M.replicateM nDraws (R.uniform 0 (100 :: Int))

randomListsDifferent :: Member RandomFu r => Int -> Sem r Bool
randomListsDifferent nDraws = do
  a <- getRandomInts nDraws
  b <- getRandomInts nDraws
  return (a /= b)

------------------------------------------------------------------------------

spec :: Spec
spec = describe "RandomFu" $ do
  it "Should produce two distinct sets of pseudo-random Ints." $ do
    result <- runM . runRandomIO $ randomListsDifferent 5
    result `shouldBe` True
  let mersenneSeed = MR.pureMT 1
  it "Should produce two distinct sets of pseudo-random ints." $ do
    result <- runM . runStatefulRandom mersenneSeed $ randomListsDifferent 5
    result `shouldBe` True
  it "Should produce two distinct sets of pseudo-random ints." $ do
    result <- runM . runAtomicStatefulRandom mersenneSeed $ randomListsDifferent 5
    result `shouldBe` True
