{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-|
Module      : Polysemy.RandomFu
Description : Polysemy random-fu effect

Polysemy "random-fu" effect.
This can be run in a few ways:
1. Directly in 'IO'
2. Using any 'Data.Random.RandomSource' from "random-fu"
3. In 'IO', using a given 'Data.Random.Source.PureMT' source.
('IO' is used to put the source in an 'IORef')

This module also contains the type-class instances to enable "absorbing"
MonadRandom, ala Polysemy.MTL.  See the tests for MTL or RandomFu for
examples of that in use.
-}

module Polysemy.RandomFu
  (
    -- * Effect
    RandomFu (..)

    -- * Actions
  , sampleRVar
#if MIN_VERSION_random_fu(0,3,0)
#else
  , getRandomPrim
#endif
  , sampleDist

    -- * Interpretations
  , runRandomIO
#if MIN_VERSION_random_fu(0,3,0)
#else
  , runRandomSource
  , runRandomIOPureMT
#endif
  )
where

import           Polysemy
import           Polysemy.State as PS
--import           Polysemy.MTL

import           Data.IORef                     ( newIORef )
import qualified Data.Random                   as R
import qualified Data.Random.RVar as R
import qualified Data.RVar                     as R (pureRVar)
import           Control.Monad.IO.Class         ( MonadIO(..) )
import Control.Monad.Reader.Class (MonadReader)
import qualified System.Random.Stateful as SR
import GHC.IORef (IORef)

------------------------------------------------------------------------------
{- | An effect capable of sampling from a "random-fu" RVar or generating a
single random-variate of any type, @t@ with a
@Data.Random.Prim t@ constructor, currently one of @Word8@, @Word16@,
@Word32@, @Word64@, @Double@ or N-byte integer.
-}
data RandomFu m r where
  SampleRVar ::  R.RVar t -> RandomFu m t
#if MIN_VERSION_random_fu(0,3,0)
#else
  GetRandomPrim :: R.Prim t -> RandomFu m t
#endif
makeSem ''RandomFu

------------------------------------------------------------------------------
-- | use the 'RandomFu` effect to sample from a "random-fu" @Distribution@.
sampleDist
  :: (Member RandomFu r, R.Distribution d t) => d t -> Sem r t
sampleDist = sampleRVar . R.rvar
{-# INLINEABLE sampleDist #-}

------------------------------------------------------------------------------
-- | Run a 'Random` effect by using the default "random-fu" 'IO' source
runRandomIO
  :: forall r a
   . MonadIO (Sem r)
  => Sem (RandomFu ': r) a
  -> Sem r a
runRandomIO x = do
  g <- SR.getStdGen
  evalState g $ reinterpret ( \case
    SampleRVar rv -> do
      g <- get
      let (x, g') = R.pureRVar rv g
      put g'
      return x
#if MIN_VERSION_random_fu(0,3,0)
#else
    GetRandomPrim pt -> raise $ R.getRandomPrim pt
#endif
    )
    x
{-# INLINEABLE runRandomIO #-}

#if MIN_VERSION_random_fu(0,3,0)
#else
------------------------------------------------------------------------------
-- | Run a 'Random' effect using a given 'R.RandomSource'
runRandomSource
  :: forall s m r a
   . (SR.StatefulGen s m
     , R.RandomSource m s
     , Member (Embed m) r
     )
  => s
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomSource source = interpret $ \case
    SampleRVar    rv -> embed $ R.runRVar rv source
    GetRandomPrim pt -> embed $ R.getRandomPrimFrom source pt
{-# INLINEABLE runRandomSource #-}

------------------------------------------------------------------------------
-- | Run in 'IO', using the given 'R.PureMT' source, stored in an 'IORef'
runRandomIOPureMT
  :: (Member (Embed IO) r
      )
  => R.PureMT
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomIOPureMT pMT = interpret $ \case
   SampleRVar    rv -> liftIO $ do
     g <- SR.newIOGenM pMT
     R.runRVar rv g
   GetRandomPrim pt -> liftIO $ do
     g <- newIORef pMT
     R.getRandomPrimFromMTRef g pt
--getRandomPrimFromMTRef   embed (SR.newIOGenM source) >>= flip runRandomSource re
{-# INLINEABLE runRandomIOPureMT #-}
#endif
