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


--- Break the parameter-mode portion of an instruction into
-- a list of single-digit paramter modes
digits :: Int -> [Int]
digits = unfoldr (Just . uncurry (flip (,)) . (`divMod` 10))

position :: Program -> Int -> Int
position p addr = p V.! addr

immediate :: Program -> Int -> Int
immediate p addr = addr

-- Should use ReaderT and WriterT to handle input and output separately...
eval :: Program -> PC -> State IntCodeIO (Program, PC)
eval p pc = do
     (input, output) <- get
     let instruction = p V.! pc
         (pmodes, opcode) = instruction `divMod` 100
         pmodeList = digits pmodes
     if opcode == 99
     then return (p, pc)  -- We're done; don't worry about incrementing the pc
     else let a1 = p V.! (pc + 1)  -- The three arguments
              a2 = p V.! (pc + 2)
              a3 = p V.! (pc + 3)
              getParameter b = case pmodeList !! b of
                                    0 -> position p
                                    1 -> immediate p
                                    2 -> error "non-positional destination"
              op1 = getParameter 0 a1
              op2 = getParameter 1 a2
              doAdd = eval (p V.// [(a3, op1 + op2)]) (pc + 4)
              doMul = eval (p V.// [(a3, op1 * op2)]) (pc + 4)
              doInp x = eval (p V.// [(a1, x)]) (pc + 2)
              doOut = eval p (pc + 2)
          in do case opcode of
                  1 -> doAdd
                  2 -> doMul
                  3 -> do let (x:xs) = input
                          put (xs, output) 
                          doInp x
                  4 -> put (input, output ++ [getParameter 0 a1]) >> doOut

disassemble :: Program -> [[Int]]
disassemble p = let x = V.head p
                    xs = V.tail p
                in  case x `mod` 100 of
                       1 -> (x: V.toList (V.take 3 xs)) : disassemble (V.drop 3 xs)
                       2 -> (x: V.toList (V.take 3 xs)) : disassemble (V.drop 3 xs)
                       3 -> (x: V.toList (V.take 1 xs)) : disassemble (V.drop 1 xs)
                       4 -> (x: V.toList (V.take 1 xs)) : disassemble (V.drop 1 xs)
                       99 -> [x] : [V.toList xs]
                       otherwise -> error $ "Unknown opcode" ++ show x


getData :: FilePath -> IO Program
getData fname = do
    contents <- readFile fname
    return $ V.fromList $ map read $ splitOn "," contents


main = do
  [fname] <- getArgs
  program <- getData fname
  let input = [1]
      output = []
  print $ execState (eval program 0) (input, output)  -- 14522484
