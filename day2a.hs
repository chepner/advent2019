import System.IO
import Data.List.Split


getData :: FilePath -> IO [Int]
getData fname = do
    contents <- readFile fname
    return $ map read $ splitOn "," contents

evalProgram :: [Int] -> [Int]
evalProgram = id

main = do
  (p0:_:_:prest) <- getData "day2a.input"
  let mod_program = p0:12:2:prest
  let (pos0:_) = evalProgram mod_program
  print pos0
  
  
