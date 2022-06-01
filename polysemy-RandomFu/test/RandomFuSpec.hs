{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RandomFuSpec where

import           Polysemy
import           Polysemy.RandomFu

import           Test.Hspec
import           Control.Monad                 as M
import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.Random                   as R

#if MIN_VERSION_random_fu(0,3,0)
#else
import           Polysemy.ConstraintAbsorber.MonadRandom
import qualified Data.Random.Source.PureMT     as R
import qualified Data.Random.Source.MWC as MWC
#endif

import qualified Data.Vector as V

getRandomInts :: Member RandomFu r => Int -> Sem r [Int]
getRandomInts nDraws =
  sampleRVar $ M.replicateM nDraws (R.uniform 0 (100 :: Int))

#if MIN_VERSION_random_fu(0,3,0)
#else
getRandomIntsMR :: R.MonadRandom m => Int -> m [Int]
getRandomIntsMR nDraws =
  R.sample $ M.replicateM nDraws (R.uniform 0 (100 :: Int))
#endif

randomListsDifferent :: Member RandomFu r => Int -> Sem r Bool
randomListsDifferent nDraws = do
  a <- getRandomInts nDraws
  b <- getRandomInts nDraws
  return (a /= b)

#if MIN_VERSION_random_fu(0,3,0)
#else
randomListsDifferentMR :: R.MonadRandom m => Int -> m Bool
randomListsDifferentMR nDraws = do
  a <- getRandomIntsMR nDraws
  b <- getRandomIntsMR nDraws
  return (a /= b)
#endif



------------------------------------------------------------------------------

spec :: Spec
spec = describe "RandomFu" $ do
  it "Should produce two distinct sets of psuedo-random Ints." $ do
    result <- runM . runRandomIO $ randomListsDifferent 5
    result `shouldBe` True
#if MIN_VERSION_random_fu(0,3,0)
#else
  it
      "Should produce [3, 78, 53, 41, 56], 5 psuedo-random Ints seeded from the same seed on each test."
    $ do
        result <- runM . runRandomIOPureMT (R.pureMT 1) $ getRandomInts 5
        result `shouldBe` [26, 56, 52, 0, 0]

  it
      "Should produce [3, 78, 53, 41, 56], 5 psuedo-random Ints seeded from the same seed on each test. "
    $ do
        result <-
          runM
          . runRandomIOPureMT (R.pureMT 1)
          $ absorbMonadRandom
          $ getRandomIntsMR 5
        result `shouldBe` [3, 78, 53, 41, 56]

  it
      "Should produce [95, 40, 24, 49, 64], 5 pseudo-random Ints seeded from the same seed on each test, from an MWC RandomSource. Absorbing MonadRandom"
    $ do
      gen <- MWC.initialize (V.fromList [0])
      result <-
        runM . runRandomSource gen $ getRandomInts 5
      result `shouldBe` [22, 15, 75, 18, 80]

  it
      "Should produce two distinct sets of psuedo-random Ints."
    $ do
        result <-
          runM . runRandomIO $ absorbMonadRandom $ randomListsDifferentMR 5
        result `shouldBe` True
#endif
