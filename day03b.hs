import System.IO
import Data.List.Split
import qualified Data.Set as S
import Control.Monad.Trans.State
import Data.Foldable
import Data.Ord
import Data.Maybe
import Data.List

data Direction  = R | L | U | D deriving (Show, Read)
data Point = Pt Int Int deriving (Eq, Show, Ord)

type Segment = (Direction, Int)
type Path = [Segment]

distance :: Point -> Point -> Int
distance (Pt x1 y1) (Pt x2 y2) = abs (x1 - x2) + abs (y1 - y2)

origin :: Point
origin = Pt 0 0

distanceFromOrigin :: Point -> Int
distanceFromOrigin = distance origin

step :: Direction -> State Point Point
step d = do
   Pt x y <- get
   let p = case d of 
             R -> Pt (x + 1) y
             L -> Pt (x - 1) y
             U -> Pt x (y + 1)
             D -> Pt x (y - 1)
   put p
   return p

segmentToDirs :: Segment -> [Direction]
segmentToDirs (dir, len) = replicate len dir

walkSegment :: Segment -> State Point [Point]
walkSegment = traverse step . segmentToDirs
  
walkPath :: Path -> State Point [Point]
walkPath p = concat <$> mapM walkSegment p

parseSegment :: String -> Segment
parseSegment (c:val) = (read [c], read val)

parsePath :: String -> Path
parsePath p = map parseSegment $ splitOn "," p

readPaths :: FilePath -> IO (Path, Path)
readPaths fname = do
  contents <- readFile fname
  let [path1, path2] = map parsePath $ lines contents
  return (path1, path2)

-- 11236 is too low
-- 11238 is right!
main = do
  (path1, path2) <- readPaths "day03.input"
  let points1 = evalState (walkPath path1) origin
      points2 = evalState (walkPath path2) origin
      common = S.intersection (S.fromList points1) (S.fromList points2)
      path_length = \x p -> fromJust (findIndex (== x) p) + 1
      f = \x -> path_length x points1 + path_length x points2
      p = minimumBy (comparing f) (S.toList common)
  print (p, f p)

