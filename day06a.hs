-- Build a tree from the input
-- Find the height of each node
-- Sum the heights


import System.Environment
import System.IO
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe

type OrbitalMap = M.Map String [String]

type Edge = (String, String)

getEdges :: FilePath -> IO [Edge]
getEdges fname = do
  contents <- fmap lines $ readFile fname
  let edgesData = map (splitOn ")") contents
  return $ map (\[x,y] -> (x, y)) edgesData

edge2map :: Edge -> OrbitalMap
edge2map (x,y) = M.singleton x [y]

makeOrbitalMap :: [Edge] -> OrbitalMap
makeOrbitalMap = M.unionsWith (++) . map edge2map

computeHeights :: OrbitalMap -> [Int]
computeHeights m = go 0 "COM"
                     where go n r = n : ((fromMaybe [] (M.lookup r m)) >>= go (n + 1))
        

  

main = do
  edges <- getEdges "day06.input"
  let orbitalMap :: OrbitalMap
      orbitalMap = makeOrbitalMap edges
  print $ orbitalMap
  print $ sum (computeHeights orbitalMap) -- 295936
  
