{-# LANGUAGE NamedFieldPuns #-}
module Language.Synthesis.Synthesis (
    Score (..), Mutation, synthesizeMhList, runningBest, Problem(..)
) where

import           Control.Monad.Random            (Rand, RandomGen)

import           Language.Synthesis.Distribution (Distr)
import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.MCMC
import           Language.Synthesis.Mutations    (Mutation)

-- | A score is anything that can be mapped to a double.
class Score a where toScore :: a -> Double

instance Score Double where toScore = id

-- | This type specifies which program to synthesize. It comes with a
-- specification, which is a program that already works, some inputs
-- and a distance function.
data Problem p s = Problem { score :: p -> s
                           , prior :: Distr p
                           , jump  :: Mutation p
                           }

-- |Given a prior distribution, score function, mutation distribution, generate
-- a list of (program, score) values through MH sampling.
synthesizeMhList :: (Score s, RandomGen gen) => Problem p s -> Rand gen [(p, s)]
synthesizeMhList Problem {prior, score, jump} = do
    first <- Distr.sample prior
    let density prog = (sc, toScore sc + Distr.logProbability prior prog)
          where sc = score prog
    list <- mhList first density jump
    return [(prog, sc) | (prog, sc, _) <- list]

-- |Given (value, score) pairs, return a running list of the best pair so far.
runningBest :: Ord s => [(a, s)] -> [(a, s)]
runningBest []           = []
runningBest (first:rest) = scanl maxScore first rest
    where maxScore (p, ps) (q, qs) | qs >= ps = (q, qs)
                                   | otherwise = (p, ps)
