{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-|
Module      : Polysemy.RandomFu
Description : Polysemy random-fu effect

Polysemy "random-fu" effect.
This can be run directly in 'IO'.
-}

module Polysemy.RandomFu
  (
    -- * Effect
    RandomFu (..)

    -- * Actions
  , sampleRVar
  , sampleDist

    -- * Interpretations
  , runRandomIO
  )
where

import           Polysemy
import           Polysemy.State as PS

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
    )
    x
{-# INLINEABLE runRandomIO #-}
