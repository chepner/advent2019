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


getData :: FilePath -> IO (V.Vector Int)
getData fname = do
    contents <- readFile fname
    return $ V.fromList $ map read $ splitOn "," contents


main = do
  program <- getData "day02.input"
  let mod_program = program V.// [(1, 12), (2, 2)]
      result = fst (eval mod_program 0)
  print $ result V.! 0  -- 3306701
  
  
