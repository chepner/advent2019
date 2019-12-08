-- Build a tree from the input
-- Find the height of each node
-- Sum the heights


import System.Environment
import System.IO
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe

type Node = String
data NodeInfo = NodeInfo { orbitedBy :: [Node] }

instance Semigroup NodeInfo where
    a <> b = NodeInfo (orbitedBy a <> orbitedBy b)

instance Monoid NodeInfo where
    mempty = NodeInfo mempty
    -- Should I redefine mconcat?
    
type OrbitalMap = M.Map Node NodeInfo

type Edge = (Node, Node)

listToEdge :: [Node] -> Edge
listToEdge [x,y] = (x,y)

getEdges :: FilePath -> IO [Edge]
getEdges fname = do
  contents <- fmap lines $ readFile fname
  let edgesData = map (splitOn ")") contents
  return $ map listToEdge edgesData

edge2map :: Edge -> OrbitalMap
edge2map (x,y) = M.singleton x (NodeInfo [y])

makeOrbitalMap :: [Edge] -> OrbitalMap
makeOrbitalMap = M.unionsWith (<>) . map edge2map

computeHeights :: OrbitalMap -> [Int]
computeHeights m = go 0 "COM"
                     where go :: Int -> Node -> [Int]
                           go n r = let childNodes = M.lookup r m
                                    in n : (fromMaybe [] (orbitedBy <$> childNodes) >>= go (n + 1))
                           
  

main = do
  edges <- getEdges "day06.input"
  let orbitalMap :: OrbitalMap
      orbitalMap = makeOrbitalMap edges
  print $ sum (computeHeights orbitalMap) -- 295936
  
