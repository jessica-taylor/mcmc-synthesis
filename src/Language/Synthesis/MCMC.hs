module Language.Synthesis.MCMC (mhList) where

import           Control.Monad
import           Control.Monad.Random            (Rand, RandomGen, getRandom,
                                                  getSplit, runRand)
import           Control.Monad.Random.Class      ()

import           Language.Synthesis.Distribution (Distr)
import qualified Language.Synthesis.Distribution as Distr


-- These functions work on triples, (value, aux, density).
-- Density functions take a value and return auxilary and density.


mhNext :: RandomGen g => (a, b, Double) -> (a -> (b, Double)) ->
          (a -> Distr a) -> Rand g (a, b, Double)
mhNext (orig, origAux, origDensity) density jump = do
    next <- Distr.sample (jump orig)
    let origToNext = Distr.logProbability (jump orig) next
        nextToOrig = Distr.logProbability (jump next) orig
        (nextAux, nextDensity) = density next
        score = nextDensity - origDensity + nextToOrig - origToNext
    acceptance <- getRandom
    return $ if score >= log acceptance
                then (next, nextAux, nextDensity)
                else (orig, origAux, origDensity)


mhList' :: RandomGen g => (a, b, Double) -> (a -> (b, Double)) ->
           (a -> Distr a) -> g -> [(a, b, Double)]
mhList' orig density jump g = orig : mhList' next density jump g'
    where (next, g') = runRand (mhNext orig density jump) g


-- |Use the Metropolis-Hastings algorithm to sample a list of values.
mhList :: RandomGen g =>
          a                          -- ^The initial value.
          -> (a -> (b, Double))      -- ^Density function.
          -> (a -> Distr a)          -- ^Jumping distribution.
          -> Rand g [(a, b, Double)] -- ^List of (value, aux, density).
mhList orig density jump =
    liftM (mhList' (orig, origAux, origDensity) density jump) getSplit
    where (origAux, origDensity) = density orig
