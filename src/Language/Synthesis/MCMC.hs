
module Language.Synthesis.MCMC (mhList) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Random.Class

import           Language.Synthesis.Distribution (Distr)
import qualified Language.Synthesis.Distribution as Distr





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


mhList :: RandomGen g => a -> (a -> (b, Double)) -> (a -> Distr a) ->
          Rand g [(a, b, Double)]
mhList orig density jump =
    liftM (mhList' (orig, origAux, origDensity) density jump) getSplit
    where (origAux, origDensity) = density orig
