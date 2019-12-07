import System.IO
import Data.List.Split
import qualified Data.Vector as V
import Debug.Trace
import System.Environment

import Control.Monad.Trans.State
import Data.List

type Program = V.Vector Int
type Addr = Int
type PC = Int
type Input = (Int, Int)
type Result = (Input, Int)
type IntCodeIO = ([Int], [Int])


digits :: Int -> [Int]
digits = unfoldr (Just . uncurry (flip (,)) . (`divMod` 10))

debug :: String -> State IntCodeIO ()
debug s = s `seq` trace s (return ())

-- Should use ReaderT and WriterT to handle input and output separately...
eval :: Program -> PC -> State IntCodeIO (Program, PC)
eval p pc = do
     (input, output) <- get
     debug $ "Program counter " ++ show pc
     let instruction = p V.! pc
         (pmodes, opcode) = instruction `divMod` 100
         pmodeList = digits pmodes
         doExit = return (p, pc)
     if opcode == 99
     then doExit
     else let src1 = p V.! (pc + 1)
              src2 = p V.! (pc + 2)
              dest = p V.! (pc + 3)
              getParameter mode = case mode of 0 -> (p V.!); 1 -> id
              op1 = getParameter (pmodeList !! 2) src1
              op2 = getParameter (pmodeList !! 1) src2
              doAdd = eval (p V.// [(dest, op1 + op2)]) (pc + 4)
              doMul = eval (p V.// [(dest, op1 * op2)]) (pc + 4)
              doInp x = eval (p V.// [(dest, x)]) (pc + 2)
              doOut = eval p (pc + 2)
          in do case opcode of
                  1 -> debug (show [opcode, src1, src2, dest]) >> doAdd
                  2 -> debug (show [opcode, src2, src2, dest]) >> doMul
                  3 -> do let (x:xs) = input
                          debug $ "Storing " ++ show x ++ " at " ++ show dest
                          put (xs, output) 
                          doInp x
                  4 -> put (input, output ++ [op1]) >> doOut


getData :: FilePath -> IO Program
getData fname = do
    contents <- readFile fname
    return $ V.fromList $ map read $ splitOn "," contents


main = do
  [fname] <- getArgs
  program <- getData fname
  let input = [1]
      output = []
  print $ runState (eval program 0) (input, output)
