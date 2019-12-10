import System.IO (readFile)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.IntMap as IM
  (Key, IntMap, insert, fromList, lookup, toList)
import Data.Bifunctor (bimap)

import Control.Monad.Trans.State (State, get, put, modify, runState)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

type Addr = IM.Key
type PC = IM.Key
type RelBase = IM.Key
type Value = Integer
type Opcode = Int
type Program = IM.IntMap Value
type IntCodeIO = ([Value], [Value])

data ProgramState = PS Program PC RelBase deriving (Show)


--- Break the parameter-mode portion of an instruction into
-- a list of single-digit paramter modes
digits :: Int -> [Int]
digits = unfoldr (Just . uncurry (flip (,)) . (`divMod` 10))

data ParameterMode = Position
                   | Immediate
                   | Relative
  deriving (Show, Enum)


position :: Program -> Addr -> Value
position = readMemory

immediate :: Program -> Addr -> Value
immediate _ addr = fromIntegral addr

relative :: Program -> RelBase -> Addr -> Value
relative p relbase addr = position p (relbase + addr)


readMemory :: Program -> Addr -> Value
readMemory p addr = fromMaybe 0 $ IM.lookup addr p

writeMemory :: Program -> Addr -> Value -> Program
writeMemory p addr v = IM.insert addr v p

asAddr :: Value -> Addr
asAddr = fromIntegral

decodeInstruction :: Value -> ([ParameterMode], Opcode)
decodeInstruction v = let decodeParamList = take 3 . map toEnum . digits . fromIntegral
                          decodeOpcode = fromIntegral
                      in bimap
                          decodeParamList
                          decodeOpcode
                          (v `divMod` 100)

-- Should use ReaderT and WriterT to handle input and output separately...
eval :: ProgramState -> Maybe Addr ->  State IntCodeIO ProgramState
eval ps@(PS p pc rb) bk = do
     let (pmodeList, opcode) = decodeInstruction (readMemory p pc)

     -- The following definitions are irrelevant if opcode is 99 (Halt)
     let a1 = readMemory p (pc + 1)  -- The three arguments, as Integers
         a2 = readMemory p (pc + 2)
         a3 = readMemory p (pc + 3)
         loadValue :: Int -> Addr -> Value
         loadValue b = case pmodeList !! b of
                               Position -> position p
                               Immediate -> immediate p
                               Relative -> relative p rb

         computeDestination b = case pmodeList !! b of
                                 Position -> asAddr
                                 Relative -> (rb +) . asAddr

         -- The values loaded for the operands of 3-argument instructions
         op1 = loadValue 0 (asAddr a1)
         op2 = loadValue 1 (asAddr a2)

         -- The instructions
         nextStep a b c = case bk of
                           Just x | x == pc -> return (PS a b c)
                           otherwise -> eval (PS a b c) bk
         -- A three-argument instruction. 
         doArith f  = let op1 = loadValue 0 (asAddr a1)
                          op2 = loadValue 1 (asAddr a2)
                          dest = computeDestination 2 a3
                          value = f op1 op2
                          p' = writeMemory p dest value
                          pc' = pc + 4
                      in nextStep p' pc' rb
         -- XXX What is *wrong* with this??
         doInput x  = let dest = computeDestination 0 a1
                          p' = writeMemory p dest x
                          pc' = pc + 2
                      in nextStep p' pc' rb
         doOutput   =    nextStep p (pc + 2) rb
         doJump tst = let op1 = loadValue 0 (asAddr a1)
                          op2 = loadValue 1 (asAddr a2)
                          pc' = if tst op1 then asAddr op2 else (pc + 3)
                      in nextStep p pc' rb
         doTest tst = let 
                          op1 = loadValue 0 (asAddr a1)
                          op2 = loadValue 1 (asAddr a2)
                          dest = computeDestination 2 a3 
                          value = if tst op1 op2 then 1 else 0
                          p' = writeMemory p dest value
                          pc' = pc + 4
                      in nextStep p' pc' rb

     case opcode of
          99 -> return ps
          1 -> doArith (+)
          2 -> doArith (*)
          3 -> do (input, output) <- get
                  let (x:xs) = input  -- XXX Should handle lack of input better
                  put (xs, output) 
                  doInput x
          4 -> do modify (fmap (++ [op1]))
                  doOutput
          5 -> doJump (/= 0)
          6 -> doJump (== 0)
          7 -> doTest (<)
          8 -> doTest (==) 
          9 -> nextStep p (pc + 2) (rb + asAddr op1)
          otherwise -> error $ "Unknown opcode " ++ show opcode ++ show pmodeList
                         ++ " at " ++ show pc

getData :: FilePath -> IO Program
getData fname = do
    contents <- readFile fname
    let bytecodes :: [Integer]
        bytecodes = map read $ splitOn "," contents
    return $ IM.fromList (zip [0..] bytecodes)


main = do
  [fname] <- getArgs
  program <- getData fname
  let initialInput = [2]
  -- 203 is too low. I'm getting output of [203, 0], though, which I think means that the opcode 203 isn't working
  -- not that the BOOST code is 203
  -- 1187721666102244 is too high. That still looks like a bad "opcode", though.
  let (PS p pc rb, (input, output)) = runState (eval (PS program 0 0) Nothing ) (initialInput, [])
  print output -- 50120
