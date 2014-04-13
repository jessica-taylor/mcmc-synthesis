{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Language.Synthesis.Mutations (
    mutateInstruction, mutateInstructionAt, mutateInstructionsAt, swapInstructions, mix, Mutation
) where

import           Control.Monad                   (foldM)
import           Control.Monad.Random            (Rand)

import           Language.Synthesis.Distribution (Distr (Distr))
import qualified Language.Synthesis.Distribution as Distr

type Mutation a = a -> Distr a

splitSelectingAt :: Int -> [a] -> ([a], a, [a])
splitSelectingAt i xs = (take i xs, xs !! i, drop (i+1) xs)

mutateInstructionAt :: Eq a => Distr a -> Int -> [a] -> Distr [a]
mutateInstructionAt instrDistr i codes = Distr (samp ()) logProb
    where (before, _, after) = splitSelectingAt i codes
          -- samp (), to get around the monomorphism restriction
          samp () = do
              elem' <- Distr.sample instrDistr
              return (before ++ [elem'] ++ after)
          logProb other =
              let (before', elem', after') = splitSelectingAt i other in
              if (before', after') == (before, after)
                  then Distr.logProbability instrDistr elem'
                  else Distr.negativeInfinity

dropAt :: [Int] -> [a] -> [a]
dropAt positions = map snd . filter ((`elem` positions) . fst) . zip [0..]

takeAt :: [Int] -> [a] -> [a]
takeAt positions = map snd . filter ((`notElem` positions) . fst) . zip [0..]

mutateInstructionsAt :: Eq a => Distr a -> [Int] -> [a] -> Distr [a]
mutateInstructionsAt instrDistr positions program = Distr (samp ()) logProb
    where instrs = Distr.replicate (length positions) instrDistr
          samp () = do
            elems <- Distr.sample instrs
            return $ foldr (uncurry replaceAt) program (zip positions elems)   
          logProb other =
            if dropAt positions other == dropAt positions program
                  then Distr.logProbability instrs $ takeAt positions other
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
