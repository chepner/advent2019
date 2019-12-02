import System.IO
import Data.List.Split
import qualified Data.Vector as V

import Control.Monad.Trans.State
import Debug.Trace

type Program = V.Vector Int
type PC = Int
type Input = (Int, Int)


eval :: Program -> PC -> (Program, PC)
eval p pc = let opcode = p V.! pc
            in case opcode of
                 99 -> (p, pc)
                 otherwise -> let src1 = p V.! (pc + 1)
                                  src2 = p V.! (pc + 2)
                                  dest = p V.! (pc + 3)
                                  op1 = p V.! src1
                                  op2 = p V.! src2
                                  f = case opcode of
                                       1 -> (+)
                                       2 -> (*)
                              in eval (p V.// [(dest, f op1 op2)]) (pc + 4)


getData :: FilePath -> IO (V.Vector Int)
getData fname = do
    contents <- readFile fname
    return $ V.fromList $ map read $ splitOn "," contents


tryIt :: Program -> Input -> (Int, Int, Int)
tryIt p (x,y) = let p' = p V.// [(1,x), (2,y)]
                in (x, y, fst (eval p' 0) V.! 0)

expectedOutput :: Int
expectedOutput = 19690720

fails :: (Int, Int, Int) -> Bool
fails (_, _, x) = x /= expectedOutput

main = do
  program <- getData "day02.input"
  let mods = [(noun,verb) | noun <- [0..1000], verb <- [0..1000]]
      ((noun,verb,_):_) = dropWhile fails $ map (tryIt program) mods
  print $ noun*100 + verb -- 7621
  
  
