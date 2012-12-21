
import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class





mhNext :: RandomGen g => (a, Double) -> (a -> Double) -> (a -> Distr a) -> 
          Rand g (a, Double)
mhNext (orig, origDensity) density jump = do
    next <- sample (jump orig)
    let origToNext = logProbability (jump orig) next
        nextToOrig = logProbability (jump next) orig
        nextDensity = density next
        score = nextDensity - origDensity + nextToOrig - origToNext
    acceptance :: Double <- getRandom
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
