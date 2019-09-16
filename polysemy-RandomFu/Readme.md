# polysemy-RandomFu v0.4.0.0

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

## Summary
- Polysemy effect and intepreters to use the random-fu library in a polysemy effect union (like an mtl stack).
- Includes a constraint "absorber" (see https://github.com/isovector/polysemy-zoo/blob/master/src/Polysemy/ConstraintAbsorber.hs) for
the random-fu ```MonadRandom``` typeclass.

## Example (from the tests)
```haskell
import           Polysemy
import           Polysemy.RandomFu

import qualified Data.Random                   as R
import qualified Data.Random.Source.PureMT     as R

getRandomInts :: Member RandomFu r => Int -> Sem r [Int]
getRandomInts nDraws =
  sampleRVar $ M.replicateM nDraws (R.uniform 0 (100 :: Int))
  
main :: IO ()
main = do
  seed <- R.newPureMT
  putStrLn . show $ runM . runRandomIOPureMT (R.pureMT seed) $ getRandomInts 5
```
should print a list of 5 pseudo-random integers. 
They will be different each time you run because the ```newPureMT``` function 
returns a different seed each time it's called.  If you replace that seed in 
the ```R.pureMT``` argument to ```runRandomIOPureMT``` with a fixed number
then you will get the *same* pseudo-random sequences each time.  This can be
useful for testing.

## Notes
- See the tests (in https://github.com/adamConnerSax/Polysemy-Extra/blob/master/polysemy-RandomFu/test/RandomFuSpec.hs) 
for more details about how to use this effect

[travis]:        <https://travis-ci.org/adamConnerSax/polysemy-extra>
[travis-badge]:  <https://travis-ci.org/adamConnerSax/polysemy-Extra.svg?branch=master>
[hackage]:       <https://hackage.haskell.org/package/polysemy-RandomFu>
[hackage-badge]: <https://img.shields.io/hackage/v/polysemy-RandomFu.svg>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/polysemy-RandomFu.svg>
[hackage-deps]: <http://packdeps.haskellers.com/feed?needle=polysemy-RandomFu>
