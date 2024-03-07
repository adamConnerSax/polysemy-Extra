{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-|
Module      : Polysemy.RandomFu
Description : Polysemy random-fu effect

Polysemy "random-fu" effect.
This can be run:
  a. Directly in 'IO'.
  b. With a given source of entropy which will be managed by Polysemy's State
  c. With a given source of entropy which will be managed by Polysemy's atomic state (using an IORef)
  d. Purely, with a given source of entropy.  This is likely not what you want since every sample will get the same
     seed.
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
  , runStatefulRandom
  , runRandomIOAtomic
  , runAtomicStatefulRandom
  )
where

import           Polysemy
import           Polysemy.State as PS
import           Polysemy.AtomicState as PAS

import qualified Data.Random                   as R
import qualified Data.RVar                     as R (pureRVar)
import qualified System.Random.Stateful as SR

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
   . Member (Embed IO) r
  => Sem (RandomFu ': r) a
  -> Sem r a
runRandomIO x = embed SR.getStdGen >>= flip runStatefulRandom x
{-# INLINEABLE runRandomIO #-}

-- | Run a 'Random` effect by using the given entropy source and managing state via Polysemy' State effect
runStatefulRandom
  :: forall r a g
  . (R.RandomGen g)
  => g
  -> Sem (RandomFu ': r) a
  -> Sem r a
runStatefulRandom g0 x = do
  evalState g0 $ reinterpret ( \case
    SampleRVar rv -> do
      g <- get
      let (x, g') = R.pureRVar rv g
      put g'
      return x
    )
    x
{-# INLINEABLE runStatefulRandom #-}

-- | Run a 'Random` effect by using the default "random-fu" 'IO' source
-- Use Atomic state to choose behavior across threads
runRandomIOAtomic
  :: forall r a
   . Member (Embed IO) r
  => Sem (RandomFu ': r) a
  -> Sem r a
runRandomIOAtomic x = embed SR.getStdGen >>= flip runAtomicStatefulRandom x

{-# INLINEABLE runRandomIOAtomic #-}

-- | Run a 'Random` effect by using the given entropy source and managing state via Polysemy' AtomicState effect (via IORef)
runAtomicStatefulRandom
  :: forall r a g
  . (Member (Embed IO) r, R.RandomGen g)
  => g
  -> Sem (RandomFu ': r) a
  -> Sem r a
runAtomicStatefulRandom g0 x = do
  fmap snd $ PAS.atomicStateToIO g0 $ reinterpret ( \case
    SampleRVar rv -> do
      g <- PAS.atomicGet
      let (x, g') = R.pureRVar rv g
      PAS.atomicPut g'
      return x
    )
    x
{-# INLINEABLE runAtomicStatefulRandom #-}


-- | Run a 'Random` effect by using a given pure source of entropy
runRandomPure
  :: forall r a g
   . R.RandomGen g
  => g
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomPure g =
  interpret (\case
                SampleRVar rv -> return $ fst $ R.pureRVar rv g
            )
{-# INLINEABLE runRandomPure #-}
