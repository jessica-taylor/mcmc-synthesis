{-# LANGUAGE RankNTypes #-}
module Language.Synthesis.Distribution (
    Distr (Distr), sample, logProbability, negativeInfinity, sumByLogs,
    categorical, uniform, randInt, replicate, mix
) where

import Prelude hiding (replicate)

import           Control.Monad        (join, replicateM)
import           Control.Monad.Random (RandomGen, Rand, getRandom, getRandomR)

data Distr a = Distr {
    sample         :: forall g. RandomGen g => Rand g a,
    logProbability :: a -> Double
}

negativeInfinity :: Double
negativeInfinity = read "-Infinity"

sumByLogs :: [Double] -> Double
sumByLogs xs = log (sum [exp (x - high) | x <- xs]) + high
    where high = maximum xs

sampleCategorical :: RandomGen g => [(a, Double)] -> Rand g a
sampleCategorical items = go items . sum $ map snd items
  where go [(x, _)] _ = return x
        go ((x, weight):rest) total = do
          acceptance <- getRandom
          if weight/total >= acceptance
            then return x
            else go rest $ total - weight

categorical :: Eq a => [(a, Double)] -> Distr a
categorical items = Distr (sampleCategorical items) logProb
    where total = sum (map snd items)
          logProb item = case lookup item items of
              Nothing -> negativeInfinity
              Just weight -> log (weight/total)

uniform :: Eq a => [a] -> Distr a
uniform xs = categorical [(x, 1.0) | x <- xs]

randInt :: (Int, Int) -> Distr Int
randInt range@(low, high) = Distr (getRandomR range) logProb
    where logProb item = if low <= item && item <= high
                            then -log (fromIntegral (high - low + 1))
                            else negativeInfinity

replicate :: Int -> Distr a -> Distr [a]
replicate n orig = Distr (replicateM n $ sample orig) logProb
    where logProb xs = sum [logProbability orig x | x <- xs]

mix :: [(Distr a, Double)] -> Distr a
mix distrs = Distr (sampleCategorical distrs >>= sample) logProb
    where logProb x = sumByLogs [log prob + logProbability distr x |
                                 (distr, prob) <- distrs] 
