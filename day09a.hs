import System.IO
import Data.List.Split
import Data.IntMap as IM
import Debug.Trace
import System.Environment
import Data.Bool

import Control.Monad.Trans.State
import Data.List

type Addr = IM.Key
type PC = IM.Key
type RelBase = IM.Key
type Value = Integer
type Program = IM.IntMap Value
type IntCodeIO = ([Value], [Value])

data ProgramState = PS Program PC RelBase


--- Break the parameter-mode portion of an instruction into
-- a list of single-digit paramter modes
digits :: Int -> [Int]
digits = unfoldr (Just . uncurry (flip (,)) . (`divMod` 10))

position :: Program -> Addr -> Value
position p addr = p IM.! addr

immediate :: Program -> Addr -> Value
immediate p addr = fromIntegral addr

relative :: Program -> RelBase -> Addr -> Value
relative p relbase addr = p IM.! (relbase + addr)


getValue :: Program -> Addr -> Value
getValue = (IM.!)

asAddr :: Value -> Addr
asAddr = fromIntegral

-- Should use ReaderT and WriterT to handle input and output separately...
eval :: ProgramState ->  State IntCodeIO ProgramState
eval (PS p pc rb) = do
     (input, output) <- get
     let instruction = getValue p pc
         (pmodes, opcode) = instruction `divMod` 100
         pmodeList = digits (fromIntegral pmodes)
     if opcode == 99
     then return (PS p pc rb)  -- We're done; don't worry about incrementing the pc
     else let a1 = p IM.! (pc + 1)  -- The three arguments
              a2 = p IM.! (pc + 2)
              a3 = p IM.! (pc + 3)
              a1addr = asAddr a1
              a2addr = asAddr a2
              a3addr = asAddr a3
              getParameter :: Int -> Addr -> Value
              getParameter b = let pmode = fromIntegral $ pmodeList !! b
                               in case pmode of
                                    0 -> position p
                                    1 -> immediate p
                                    2 -> relative p rb
                                    otherwise -> error $ "Unknown parameter mode " ++ show pmode
              op1 = getParameter 0 a1addr
              op2 = getParameter 1 a2addr
              doAdd = eval $ PS (IM.insert a3addr (op1 + op2) p) (pc + 4) rb
              doMul = eval $ PS (IM.insert a3addr (op1 * op2) p) (pc + 4) rb
              doInp x = eval $ PS (IM.insert a1addr x p) (pc + 2) rb
              doOut = eval $ PS p (pc + 2) rb
              doJump tst = eval $ PS p (bool (pc + 3) (asAddr op2) (tst op1)) rb
              doTest tst = let p' = IM.insert a3addr (fromIntegral $ fromEnum (tst op1 op2)) p
                           in eval $ PS p' (pc + 4) rb
          in do case opcode of
                  1 -> doAdd
                  2 -> doMul
                  3 -> do let (x:xs) = input
                          put (xs, output) 
                          doInp x
                  4 -> do put (input, output ++ [getParameter 0 a1addr])
                          doOut
                  5 -> doJump (/= 0)
                  6 -> doJump (== 0)
                  7 -> doTest (<)
                  8 -> doTest (==) 
                  9 -> eval $ PS p (pc + 2) (rb + fromIntegral op1)

getData :: FilePath -> IO Program
getData fname = do
    contents <- readFile fname
    let bytecodes :: [Integer]
        bytecodes = Prelude.map read $ splitOn "," contents
    return $ IM.fromList (zip [0..] bytecodes)


main = do
  [fname] <- getArgs
  program <- getData fname
  let input = [1]
      output = []
  -- 203 is too low. I'm getting output of [203, 0], though, which I think means that the opcode 203 isn't working
  -- not that the BOOST code is 203
  print $ execState (eval $ PS program 0 0) (input, output)
