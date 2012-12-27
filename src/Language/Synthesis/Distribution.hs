{-# LANGUAGE RankNTypes #-}
module Language.Synthesis.Distribution (
    Distr (Distr), sample, logProbability, negativeInfinity, sumByLogs,
    categorical, uniform, randInt, replicate, mix
) where

import           Prelude              hiding (replicate)

import           Control.Monad        (replicateM)
import           Control.Monad.Random (Rand, Random, RandomGen, getRandom,
                                       getRandomR)

-- |Represents a discrete probability distribution.
data Distr a = Distr {
    -- |Sample a random item from the distribution.
    sample         :: forall g. RandomGen g => Rand g a,
    -- |Compute the log probability of a given value.
    logProbability :: a -> Double
}

-- |Negative infinity, the log of 0 probability.
negativeInfinity :: Double
negativeInfinity = read "-Infinity"

-- |Computes (log . sum . map exp), with more numeric precision.
sumByLogs :: [Double] -> Double
sumByLogs xs = log (sum [exp (x - high) | x <- xs]) + high
    where high = maximum xs

-- |Samples from an (item, weight) list.
sampleCategorical :: RandomGen g => [(a, Double)] -> Rand g a
sampleCategorical items = go items . sum $ map snd items
  where go [] _ = error "Cannot sample from an empty list."
        go [(x, _)] _ = return x
        go ((x, weight):rest) total = do
          acceptance <- getRandom
          if weight/total >= acceptance
            then return x
            else go rest $ total - weight

-- |A distribution from an (item, weight) list.
categorical :: Eq a => [(a, Double)] -> Distr a
categorical items = Distr (sampleCategorical items) logProb
    where total = sum (map snd items)
          logProb item = case lookup item items of
              Nothing -> negativeInfinity
              Just weight -> log (weight/total)

-- |Uniform distribution.
uniform :: Eq a => [a] -> Distr a
uniform xs = categorical [(x, 1.0) | x <- xs]

-- |A distribution over some integral type, inclusively between the 2
-- values.
randInt :: (Integral i, Random i) => (i, i) -> Distr i
randInt range@(low, high) = Distr (getRandomR range) logProb
    where logProb item = if low <= item && item <= high
                            then -log (fromIntegral $ high - low + 1)
                            else negativeInfinity

-- |Generate n independent draws from a distribution.
replicate :: Int -> Distr a -> Distr [a]
replicate n orig = Distr (replicateM n $ sample orig) logProb
    where logProb xs = sum [logProbability orig x | x <- xs]

-- |Given (distribution, weight) pairs, mix the distributions.
mix :: [(Distr a, Double)] -> Distr a
mix distrs = Distr (sampleCategorical distrs >>= sample) logProb
    where logProb x = sumByLogs [log prob + logProbability distr x |
                                 (distr, prob) <- distrs]
