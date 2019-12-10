import System.IO (readFile)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.IntMap as IM
  (Key, IntMap, insert, fromList, lookup, toList)
import Data.Bifunctor (bimap)

import Control.Monad.Trans.State (State, get, put, modify, runState)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

type Map = [[Char]]

getData :: FilePath -> IO Map
getData fname = do
    contents <- readFile fname
    let rows = lines contents
    return rows

main = do
  [fname] <- getArgs
  map <- getData fname
  print map
