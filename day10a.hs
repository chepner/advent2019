import System.IO (readFile)
import System.Environment (getArgs)
import Data.Ratio

import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Data.Ord

type Map = [[Char]]
data Point = Pt {x :: Int, y :: Int} deriving (Show, Eq)

getData :: FilePath -> IO Map
getData fname = do
    contents <- readFile fname
    let rows = lines contents
    return rows


locateAsteroids :: Map -> [Point]
locateAsteroids m = let y = length m
                        x = length (m !! 0)
                        coords = [Pt i j | i <- [0..x-1], j <- [0..y-1]] 
                    in [t | t@(Pt x y) <- coords, (m !! y) !! x == '#']


getAsteroids :: FilePath -> IO [Point]
getAsteroids fname = locateAsteroids <$> getData fname

inBox :: Point -> Point -> Point -> Bool
inBox p1@(Pt x1 y1) p2@(Pt x2 y2) = let ul = Pt (min x1 x2) (min y1 y2)
                                        lr = Pt (max x1 x2) (max y1 y2)
                                    in foo ul lr
  where foo ul lr p = and $ [x ul <= x p
                            ,x p <= x lr
                            ,y ul <= y p
                            ,y p <= y lr]


-- Nothing represents the slope of a vertical line
-- Just x represents the rest
slope :: Point -> Point -> Maybe (Ratio Int)
slope p1 p2 = if x p2 == x p1 then Nothing else Just $ (y p2 - y p1) % (x p2 - x p1)


-- Eq a => Eq (Maybe a), so no need to address Nothing values directly
colinear :: Point -> Point -> Point -> Bool
colinear p1 p2 p3 = let m1 = slope p1 p2
                        m2 = slope p2 p3
                        m3 = slope p1 p3
                    in m1 == m2 && m2 == m3


canSee :: Point -> Point -> Reader [Point] Bool
p1 `canSee` p2 = do
   asteroids <- ask
   let possibleBlockers = filter (colinear p1 p2)
                          . filter (inBox p1 p2)
                          . filter (/= p1)
                          . filter (/= p2) $ asteroids
   return (null possibleBlockers)


countVisible :: Point -> Reader [Point] Int
countVisible station = do
  asteroids <- ask
  length <$> (filterM (station `canSee`) (filter (/= station) asteroids))


main = do
  [fname] <- getArgs
  asteroids <- getAsteroids fname
  print $ length asteroids
  let countFromStation s = (s, runReader (countVisible s) asteroids)
  let counts = map countFromStation asteroids
  -- not 12 or 14. Seems like it should be much higher
  -- 293 is too high
  -- 292 is just right. (Yes, I counted a station location as being visible to itself)
  print (maximumBy (comparing snd) counts)
  print "Done"
