module Language.Synthesis.Forth (
) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Random.Class

import           Language.Synthesis.Distribution (Distr (Distr))
import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.MCMC
import           Language.Synthesis.Synthesis


type TestCase = (ForthData, ForthData)

data Problem = Problem {
    testCases :: [TestCase],
    distanceFunction :: ForthData -> ForthData -> Double
}

settings :: Int -> Settings Instruction
settings numCodes = Settings { 
    numOpcodes = numCodes,
    instructionDistr = 
    mutationWeights = [(1.0, mutateInstruction),
                       (1.0, swapInstructions)]
}

runProgram :: [Instruction] -> ForthData -> ForthData


getScore :: Problem -> [Instruction] -> Double
getScore problem program =
    logProbability (priorDistr settings) program +
    sum [distanceFunction problem (runProgram program input) output |
         (input, output) <- testCases problem]

