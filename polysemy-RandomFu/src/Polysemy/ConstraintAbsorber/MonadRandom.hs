{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE TemplateHaskell             #-}

module Polysemy.ConstraintAbsorber.MonadRandom
  (
    absorbMonadRandom
  )
where

import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.RandomFu

import qualified Data.Random.Source            as R
import qualified Data.Random.Internal.Source   as R

------------------------------------------------------------------------------
-- | A dictionary of the functions we need to supply
-- to make an instance of MonadRandom
data RandomDict m = RandomDict { getRandomPrim_ :: forall t. R.Prim t -> m t }

-- | Wrapper for a monadic action with phantom
-- type parameter for reflection.
-- Locally defined so that the instance we are going
-- to build with reflection must be coherent, that is
-- there cannot be orphans.
newtype Action m s' a = Action { action :: m a }
  deriving (Functor, Applicative, Monad)

-- | Given a reifiable mtl Error dictionary,
-- we can make an instance of @MonadRandom@ for the action
-- wrapped in @Action@.
$(R.monadRandom [d|
      instance ( Monad m
               , Reifies s' (RandomDict m)
               ) => R.MonadRandom (Action m s') where
          getRandomPrim t = Action
            $ getRandomPrim_ (reflect $ Proxy @s') t
          {-# INLINEABLE getRandomPrim #-}
  |])


-- | absorb a @MonadError e@ constraint into @Member (Error e) r => Sem r@
absorbMonadRandom :: forall r a. (Member RandomFu r)
  => (R.MonadRandom (Sem r) => Sem r a)
  -> Sem r a
absorbMonadRandom = absorbWithSem @R.MonadRandom @Action
  (RandomDict (getRandomPrim @r))
  (Sub Dict)
{-# INLINEABLE absorbMonadRandom #-}
