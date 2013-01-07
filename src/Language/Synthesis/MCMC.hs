module Language.Synthesis.MCMC (mhList) where

import           Control.Monad.Random            (Rand, RandomGen, getRandom,
                                                  getSplit, runRand)

import           Data.Functor                    ((<$>))

import           Language.Synthesis.Distribution (Distr)
import qualified Language.Synthesis.Distribution as Distr


-- These functions work on triples, (value, aux, density).
-- Density functions take a value and return auxilary and density.

-- |Use the Metropolis-Hastings algorithm to sample a list of values.
mhList :: RandomGen g =>
          a                         -- ^The initial value.
          -> (a -> (b, Double))       -- ^Density function.
          -> (a -> Distr a)           -- ^Jumping distribution.
          -> Rand g [(a, b, Double)] -- ^List of (value, aux, density).
mhList startValue density jump = go (startValue, startAux, startDensity) <$> getSplit
  where (startAux, startDensity) = density startValue
        go orig g = let (next, g') = runRand (mhNext orig) g in orig : go next g'
        mhNext (orig, origAux, origDensity) = do
            next <- Distr.sample $ jump orig
            let origToNext = Distr.logProbability (jump orig) next
                nextToOrig = Distr.logProbability (jump next) orig
                (nextAux, nextDensity) = density next
                score = nextDensity - origDensity + nextToOrig - origToNext
            acceptance <- getRandom
            return $ if score >= log acceptance
                        then (next, nextAux, nextDensity)
                        else (orig, origAux, origDensity)
