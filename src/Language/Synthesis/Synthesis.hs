module Language.Synthesis.Synthesis (
    Mutation, Settings (Settings),
    numInstructions, instructionDistr, mutationWeights,
    priorDistr, mutationDistr, mutateInstruction, swapInstructions
) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Random.Class

import           Language.Synthesis.Distribution (Distr (Distr))
import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.MCMC


type Mutation a = a -> Distr a

synthesizeMhList :: Randomgen g => Distr a -> (a -> Double) -> Mutation -> Rand g [(a, Double)]
synthesizeMhList prior score jump = do
    first <- Distr.sample prior
    let density prog = (sc, sc + logProbability prior prog)
            where sc = score prog
    list <- mhList first density jump
    return [(prog, sc) | (prog, sc, _) <- list]

runningBest :: [(a, Double)] -> [(a, Double)]
runningBest (first:rest) = scanl first maxScore rest
    where maxScore (p, ps) (q, qs) | qs >= ps = (q, qs)
                                   | otherwise = (p, ps)


