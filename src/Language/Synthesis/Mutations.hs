module Language.Synthesis.Mutations (
    mutateInstruction, swapInstructions, mix
) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Random.Class

import           Language.Synthesis.Distribution (Distr (Distr))
import qualified Language.Synthesis.Distribution as Distr
import           Language.Synthesis.Synthesis    (Mutation)


splitSelectingAt :: Int -> [a] -> ([a], a, [a])
splitSelectingAt i xs = (take i xs, xs !! i, drop (i+1) xs)

mutateInstructionAt :: Eq a => Distr a -> Int -> [a] -> Distr [a]
mutateInstructionAt instrDistr i codes = Distr (samp ()) logProb
    where (before, elem, after) = splitSelectingAt i codes
          -- samp (), to get around the monomorphism restriction
          samp () = do
              elem' <- Distr.sample instrDistr
              return (before ++ [elem] ++ after)
          logProb other =
              let (before', elem', after') = splitSelectingAt i other in
              if (before', after') == (before, after)
                  then Distr.logProbability instrDistr elem'
                  else Distr.negativeInfinity

-- |Given a distribution over instructions, mutates a random instruction.
mutateInstruction :: Eq a => Distr a -> Mutation [a]
mutateInstruction instrDistr codes =
    Distr.mix [(mutateInstructionAt instrDistr i codes, 1.0) | i <- [0 .. length codes - 1]]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = before ++ [x] ++ after
    where (before, _, after) = splitSelectingAt i xs

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = replaceAt i (xs !! j) $ replaceAt j (xs !! i) xs

-- |Swaps 2 random instructions.
swapInstructions :: Eq a => Mutation [a]
swapInstructions codes =
    Distr.uniform [swapAt i j codes | i <- [1 .. length codes - 1], j <- [0 .. i - 1]]

-- |Given (mutation, weight) list, mixes the mutations.
mix :: [(Mutation a, Double)] -> Mutation a
mix muts program = Distr.mix [(mut program, weight) | (mut, weight) <- muts]
