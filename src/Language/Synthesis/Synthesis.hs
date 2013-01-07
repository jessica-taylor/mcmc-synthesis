{-# LANGUAGE NamedFieldPuns #-}
module Language.Synthesis.Synthesis (
    Mutation, synthesizeMhList, runningBest, Problem(..)
) where

import           Control.Monad.Random            (Rand, RandomGen)

import           Language.Synthesis.Distribution (Distr)
import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.MCMC
import           Language.Synthesis.Mutations    (Mutation)

-- | This type specifies which program to synthesize. It comes with a
-- specification, which is a program that already works, some inputs
-- and a distance function.
data Problem program = Problem { score :: program -> Double
                               , prior :: Distr program
                               , jump  :: Mutation program
                               }

-- |Given a prior distribution, score function, mutation distribution, generate
-- a list of (program, score) values through MH sampling.
synthesizeMhList :: RandomGen gen => Problem program -> Rand gen [(program, Double)]
synthesizeMhList Problem {prior, score, jump} = do
    first <- Distr.sample prior
    let density prog = (sc, sc + Distr.logProbability prior prog)
            where sc = score prog
    list <- mhList first density jump
    return [(prog, sc) | (prog, sc, _) <- list]

-- |Given (value, score) pairs, return a running list of the best pair so far.
runningBest :: [(a, Double)] -> [(a, Double)]
runningBest []           = []
runningBest (first:rest) = scanl maxScore first rest
    where maxScore (p, ps) (q, qs) | qs >= ps = (q, qs)
                                   | otherwise = (p, ps)
