import System.IO
import Data.List.Split

type Segment = (Char, Int)
type Path = [Segment]

parseSegment :: String -> Segment
parseSegment (c:val) = (c, read val)


parsePath :: String -> Path
parsePath p = map parseSegment $ splitOn "," p


readPaths :: FilePath -> IO (Path, Path)
readPaths fname = do
  contents <- readFile fname
  let [path1, path2] = map parsePath $ lines contents
  return (path1, path2)
  

main = do
  (path1, path2) <- readPaths "day03.input"
  print path1
  print path2

