-- Build a tree from the input
-- Find the height of each node
-- Sum the heights


import System.Environment
import System.IO
import Data.List.Split
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Tuple

-- Graph primitives
type Node = String
type Edge = (Node, Node)


listToEdge :: [Node] -> Edge
listToEdge [x,y] = (x,y)

getEdges :: FilePath -> IO [Edge]
getEdges fname = do
  contents <- fmap lines $ readFile fname
  let edgesData = map (splitOn ")") contents
  return $ map listToEdge edgesData


findPath :: [Edge] -> Node -> [Edge]
findPath edges n = go n
     where reversedEdges = map swap edges
           go :: Node -> [Edge]
           go "COM" = []
           go r = case lookup r reversedEdges of
                   Just p -> (r, p): go p


joinEdges :: Edge -> Edge -> [Node]
joinEdges x@(a,b) y@(c,d) | b == c = [a,d]


flatPath :: [Edge] -> [Node]
flatPath [] = []
flatPath (x:xs) = fst x : map snd xs



main = do
  edges <- getEdges "day06.input"
  let youPath = flatPath $ findPath edges "YOU"
      santaPath = flatPath $ findPath edges "SAN"
      i = intersect youPath santaPath
  print $ i
  print $ length (youPath \\ i) + length (santaPath \\ i) -- 459 too high. 457!
  
