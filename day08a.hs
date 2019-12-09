import System.IO
import System.Environment
import Data.Char
import Data.List.Split
import Data.List
import Data.Ord

import Control.Monad.Trans.State

type Image = [Int]
type Layer = [Int]
getData :: FilePath -> Int -> Int -> IO [Layer]
getData fname x y = do
    contents <- readFile fname
    return $ getLayers x y $ map (\c -> ord(c) - ord('0')) (init contents)


getLayers :: Int -> Int -> Image ->  [Layer]
getLayers x y = chunksOf (x * y)

countValues :: Layer -> (Int, Int, Int)
countValues [] = (0,0,0)
countValues (p:ps) = let (x,y,z) = countValues ps
                     in case p of
                         0 -> (x+1, y, z)
                         1 -> (x, y+1, z)
                         2 -> (x, y, z+1)

main = do
  [fname] <- getArgs
  image <- getData fname 25 6
  
  let (_, y, z) = minimumBy (comparing (\(x,y,z) -> x)) $ map countValues image
  print (y * z) -- 1950
  
