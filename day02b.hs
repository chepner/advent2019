import System.IO
import Data.List.Split
import qualified Data.Vector as V

import Control.Monad.Trans.State

type Program = V.Vector Int
type PC = Int
type Input = (Int, Int)
type Result = (Input, Int)

{-
type ProgramState = State (Program, PC)

evalStep :: ProgramState ()
evalStep ps = do
    (p, pc) <- ps
    let opcode = p V.! pc
    case opcode of
      99 -> return ()
      otherwise = let src1 = p V.! (pc + 1)
                      src2 = p V.! (pc + 2)
                      dest = p V.! (pc + 3)
                      op1 = p V.! src1
                      op2 = p V.! src2
                      f = case opcode of
                           1 -> (+)
                           2 -> (*)
                      p' = p V.// [(dest, f op1 op2)]
                      pc' = pc + 4
                    in put (p', pc')
-}


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


getData :: FilePath -> IO Program
getData fname = do
    contents <- readFile fname
    return $ V.fromList $ map read $ splitOn "," contents


tryIt :: Program -> Input -> Result
tryIt p inp@(x,y) = let p' = p V.// [(1,x), (2,y)]
                in (inp, fst (eval p' 0) V.! 0)

expectedOutput :: Int
expectedOutput = 19690720

search :: Program -> Int -> State [Input] Int
search p g = do
  mods <- get
  let (success:_) = dropWhile (\(_,x) -> x /= g) $ map (tryIt p) mods
      ((noun, verb), _) = success
  return $ noun * 100 + verb

main = do
  program <- getData "day02.input"
  let mods = [(noun,verb) | noun <- [0..100], verb <- [0..100]]
  print $ evalState (search program expectedOutput) mods
  
  
