import System.IO
import Data.List.Split
import qualified Data.Vector as V

import Control.Monad.Trans.State
import Debug.Trace

type Program = V.Vector Int
type PC = Int


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


getData :: FilePath -> IO [Int]
getData fname = do
    contents <- readFile fname
    return $ map read $ splitOn "," contents


main = do
  (p0:_:_:prest) <- getData "day2a.input"
  let mod_program = p0:12:2:prest
      program = V.fromList mod_program
      result = fst (eval program 0)
  print $ result V.! 0  -- 3306701
  
  
