
module Language.Synthesis.MCMC where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class

import Language.Synthesis.Distribution (Distr)
import qualified Language.Synthesis.Distribution as Distr





mhNext :: RandomGen g => (a, Double) -> (a -> Double) -> (a -> Distr a) -> 
          Rand g (a, Double)
mhNext (orig, origDensity) density jump = do
    next <- Distr.sample (jump orig)
    let origToNext = Distr.logProbability (jump orig) next
        nextToOrig = Distr.logProbability (jump next) orig
        nextDensity = density next
        score = nextDensity - origDensity + nextToOrig - origToNext
    acceptance <- getRandom
    return $ if score >= log acceptance 
                then (next, nextDensity)
                else (orig, origDensity)


mhList' :: RandomGen g => (a, Double) -> (a -> Double) -> (a -> Distr a) -> 
           g -> [(a, Double)]
mhList' orig density jump g = orig : mhList' next density jump g'
    where (next, g') = runRand (mhNext orig density jump) g


mhList :: RandomGen g => a -> (a -> Double) -> (a -> Distr a) -> 
          Rand g [(a, Double)]
mhList orig density jump = 
    liftM (mhList' (orig, density orig) density jump) getSplit
