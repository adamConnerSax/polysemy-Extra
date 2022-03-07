{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications       #-}
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
  , getRandomPrim
  , sampleDist

    -- * Interpretations
  , runRandomSource
  , runRandomIO
  , runRandomIOPureMT
  )
where

import           Polysemy
--import           Polysemy.MTL

import           Data.IORef                     ( newIORef )
import qualified Data.Random                   as R
import qualified Data.Random.Internal.Source   as R
import qualified Data.Random.Source.StdGen     as R
import qualified Data.Random.Source.PureMT     as R
import qualified Data.RVar                     as R (pureRVar)
import Data.Random.Source.IO ()
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
  GetRandomPrim :: R.Prim t -> RandomFu m t

makeSem ''RandomFu

------------------------------------------------------------------------------
-- | use the 'RandomFu` effect to sample from a "random-fu" @Distribution@.
sampleDist
  :: (Member RandomFu r, R.Distribution d t) => d t -> Sem r t
sampleDist = sampleRVar . R.rvar
{-# INLINEABLE sampleDist #-}

------------------------------------------------------------------------------
-- | Run a 'Random' effect using a given 'R.RandomSource'
runRandomSource
  :: forall s m r a
   . ( R.RandomSource m s
     , R.StatefulGen s m
     , MonadReader s m
     , MonadReader s R.RVar
     , R.StatefulGen s R.RVar
     , R.MonadRandom R.RVar
     , Member (Embed m) r
     )
  => s
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomSource source = interpret $ \case
    SampleRVar    rv -> embed $ R.runRVar (R.sample rv) source
    GetRandomPrim pt -> embed $ R.runRVar (R.getRandomPrim pt) source
{-# INLINEABLE runRandomSource #-}

------------------------------------------------------------------------------
-- | Run a 'Random` effect by using the default "random-fu" 'IO' source
runRandomIO
  :: forall r a
   . (MonadIO (Sem r)
--     , R.Distribution R.RVar a
--     , R.StatefulGen s IO
     )
  => Sem (RandomFu ': r) a
  -> Sem r a
runRandomIO =
  interpret $ \case
    SampleRVar    rv -> liftIO $ do
      g <- SR.getStdGen
      return $ fst $ R.pureRVar rv g
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt

{-# INLINEABLE runRandomIO #-}

------------------------------------------------------------------------------
-- | Run in 'IO', using the given 'R.PureMT' source, stored in an 'IORef'
runRandomIOPureMT
  :: (Member (Embed IO) r
     , MonadReader (IORef R.PureMT) R.RVar
     , MonadReader (IORef R.PureMT) IO
     , R.StatefulGen (IORef R.PureMT) R.RVar
     , R.StatefulGen (IORef R.PureMT) IO
     , R.MonadRandom R.RVar
      )
  => R.PureMT
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomIOPureMT source re =
  embed (newIORef source) >>= flip runRandomSource re
{-# INLINEABLE runRandomIOPureMT #-}
