module Language.Synthesis.Synthesis (
    Mutation, Settings (Settings),
    numOpcodes, opcodeDistr, mutationWeights,
    priorDistr, mutationDistr, mutateOpcode, swapOpcodes
) where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Random.Class

import           Language.Synthesis.Distribution (Distr (Distr))
import qualified Language.Synthesis.Distribution as Distr


type Mutation a = Settings a -> [a] -> Distr [a]


data Settings a = Settings {
    numOpcodes      :: Int,
    opcodeDistr     :: Distr a,
    mutationWeights :: [(Mutation a, Double)]
}

priorDistr :: Settings a -> Distr [a]
priorDistr settings = Distr.replicate (numOpcodes settings) (opcodeDistr settings)

mutationDistr :: Settings a -> [a] -> Distr [a]
mutationDistr settings orig =
    Distr.mix [(m settings orig, weight) | (m, weight) <- mutationWeights settings]

splitSelectingAt :: Int -> [a] -> ([a], a, [a])
splitSelectingAt i xs = (take i xs, xs !! i, drop (i+1) xs)

mutateOpcodeAt :: Eq a => Settings a -> Int -> [a] -> Distr [a]
mutateOpcodeAt settings i codes = Distr (samp ()) logProb
    where (before, elem, after) = splitSelectingAt i codes
          -- samp (), to get around the monomorphism restriction
          samp () = do
              elem' <- Distr.sample (opcodeDistr settings)
              return (before ++ [elem] ++ after)
          logProb other =
              let (before', elem', after') = splitSelectingAt i other in
              if (before', after') == (before, after)
                  then Distr.logProbability (opcodeDistr settings) elem'
                  else Distr.negativeInfinity

mutateOpcode :: Eq a => Mutation a
mutateOpcode settings codes = 
    Distr.mix [(mutateOpcodeAt settings i codes, 1.0) | i <- [0 .. length codes - 1]]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = before ++ [x] ++ after
    where (before, _, after) = splitSelectingAt i xs

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = replaceAt i (xs !! j) $ replaceAt j (xs !! i) xs


swapOpcodes :: Eq a => Mutation a
swapOpcodes settings codes =
    Distr.uniform [swapAt i j codes | i <- [1 .. length codes - 1], j <- [0 .. i - 1]]
