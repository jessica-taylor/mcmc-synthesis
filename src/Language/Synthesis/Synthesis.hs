
import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class


data Mutation = MutateOpcode | SwapOpcodes 


data Settings = Settings {
    numOpcodes :: Int,
    opcodeWeights :: (OpCode, Double)
    mutationWeights :: (Mutation, Double)
}

opcodeDistr :: Settings -> Distr OpCode
opcodeDistr = Distr.categorical . opcodeWeights

priorDistr :: Settings -> Distr [OpCode]
priorDistr settings = Distr.replicate (numOpcodes settings) (opcodeDistr settings)

splitAt :: Int -> [a] -> ([a], a, [a])
splitAt i xs = (take i xs, xs !! i, drop (i+1) xs)

mutateOpcodeAt :: Settings -> Int -> [OpCode] -> Distr [OpCode]
mutateOpcodeAt settings i codes = Distr samp logProbability
    where (before, elem, after) = splitAt i codes
          samp = do
              elem' <- opcodeDistr settings
              return (before ++ [elem] ++ after)
          logProbability other = do
              let (before', elem', after') = splitAt i other
              if (before', after') == (before, after)
                  then logProbability (opcodeDistr settings) elem'
                  else negativeInfinity

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = before ++ [x] ++ after
    where (before, _, after) = splitAt i xs

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = replaceAt i (xs !! j) $ replaceAt j (xs !! i) xs

mutate :: Settings -> Mutation -> [OpCode] -> Distr [OpCode]
mutate settings MutateOpcode codes = 
    Distr.mix [(mutateOpcodeAt settings i codes, 1.0) | i <- [0 .. length codes - 1]]
mutate settings SwapOpcodes codes = 
    Distr.uniform [swapAt i j codes | i <- [1 .. length codes - 1], j <- [0 .. i - 1]]

jumpDistr :: Settings -> [OpCode] -> Distr [OpCode]
jumpDistr settings orig = 
    Distr.mix [(mutate m orig, weight) | (m, weight) <- mutationWeights settings]

