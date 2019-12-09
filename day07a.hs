import System.IO
import Data.List.Split
import qualified Data.Vector as V
import Debug.Trace
import System.Environment
import Data.Bool

import Control.Monad.Trans.State
import Data.List
import Data.Ord

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
              doTrueJump = eval p $ bool (pc + 3) op2 (op1 /= 0)
              doFalseJump = eval p $ flip bool (pc + 3) op2 (op1 /= 0)
          in do case opcode of
                  1 -> doAdd
                  2 -> doMul
                  3 -> do let (x:xs) = input
                          put (xs, output) 
                          doInp x
                  4 -> put (input, output ++ [getParameter 0 a1]) >> doOut
                  5 -> doTrueJump
                  6 -> doFalseJump
                  7 -> eval (p V.// [(a3, fromEnum (op1 < op2))]) (pc + 4)
                  8 -> eval (p V.// [(a3, fromEnum (op1 == op2))]) (pc + 4)

getData :: FilePath -> IO Program
getData fname = do
    contents <- readFile fname
    return $ V.fromList $ map read $ splitOn "," contents


runWithAmplifiers :: Program -- Program
                   -> [Int]  -- amplifier order
                   -> ([Int], Int) -- output
runWithAmplifiers p inp@[a1,a2,a3,a4,a5] = let result0 = 0
                                               (_, [result1]) = execState (eval p 0) ([a1, result0], [])
                                               (_, [result2]) = execState (eval p 0) ([a2, result1], [])
                                               (_, [result3]) = execState (eval p 0) ([a3, result2], [])
                                               (_, [result4]) = execState (eval p 0) ([a4, result3], [])
                                               (_, [result5]) = execState (eval p 0) ([a5, result4], [])
                                           in (inp, result5)


main = do
  [fname] <- getArgs
  program <- getData fname
  let inputs = permutations [0,1,2,3,4]
  -- traverse (print . runWithAmplifiers program) inputs
  -- 212 too low
  print $ maximumBy (comparing (\(_,o) -> o)) (map (runWithAmplifiers program) inputs) -- 225056
