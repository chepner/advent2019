import System.IO (readFile)
import System.Environment (getArgs)

import Control.Arrow ((>>>))
import Data.List(sortBy, groupBy, uncons)
import Data.Ord (comparing)
import Data.Function (on, (&))
import Data.Maybe (catMaybes)
import Data.Ratio (Ratio, (%))
import Data.Ord -- How do I import just Down?

type Map = [[Char]]
data Point = Pt {x :: Int, y :: Int} deriving (Show, Eq)

data Quadrant = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Show)

data Slope = VerticalUp    -- dx == 0, dy >= 0
           | VerticalDown  -- dx == 0, dy < 0
           | Slope Int Int
  deriving (Show)

instance Eq Slope where
  VerticalUp == VerticalUp = True
  VerticalDown == VerticalDown = True
  Slope x1 y1 == Slope x2 y2 = x1 * y2 == x2 * y1
  _ == _ = False

-- XXX Sorting inside Q3 and Q4 is reversed
instance Ord Slope where
  VerticalUp <= _ = True
  VerticalDown <= VerticalUp = False
  VerticalDown <= VerticalDown = True
  VerticalDown <= m = let q = getQuadrantFromSlope m
                      in q == Q3 || q == Q4
  Slope _ _ <= VerticalUp = False
  Slope _ dx <= VerticalDown = dx >= 0
  m1 <= m2 | getQuadrantFromSlope m1 < getQuadrantFromSlope m2 = True
           | getQuadrantFromSlope m1 > getQuadrantFromSlope m2 = False
  Slope dy1 dx1 <= Slope dy2 dx2 = (dy1 % dx1) >= (dy2 % dx2)


getQuadrantFromSlope :: Slope -> Quadrant
getQuadrantFromSlope VerticalUp = Q1
getQuadrantFromSlope VerticalDown = Q3
getQuadrantFromSlope (Slope dy dx) | dx > 0 && dy >= 0 = Q1
                                   | dx > 0 && dy < 0 = Q2
                                   | dx <= 0 && dy <= 0 = Q3
                                   | dx <= 0 && dy > 0 = Q4

getSlope :: Point -> Slope
getSlope (Pt 0 y) | y >= 0 = VerticalUp
                  | y < 0 = VerticalDown
getSlope (Pt x y) = Slope y x

getQuadrant :: Point -> Quadrant
getQuadrant = getQuadrantFromSlope . getSlope



pointOrder :: Point -> Point -> Ordering
pointOrder p1 p2 = clockwiseOrder p1 p2
                   <> linearOrder p1 p2
  where d (Pt x y) = x*x + y*y  -- distance from origin
        clockwiseOrder = compare `on` getSlope
        linearOrder = compare `on` d
        

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


translate :: Point -> Point -> Point
translate o p = Pt (x p - x o) (y p - y o)

roundRobin :: [[Point]] -> [Point]
roundRobin [] = []
roundRobin xs = (++) <$> getFirst <*> recurse $ catMaybes $ map uncons xs
  where getFirst = map fst
        getRest = map snd
        recurse = roundRobin . getRest


type Heading = Double
getHeading :: Point -> Double
getHeading (Pt x y) = let h = atan2 (negate y') x' + pi/2
                      in h + if h < 0 then pi else 0
  where conv = fromRational . fromIntegral
        y' = conv y
        x' = conv x

simpleCompare :: Point -> Point -> Ordering
simpleCompare p1 p2 = compare (getHeading p1) (getHeading p2)

main = do
    [fname] <- getArgs
    asteroids <- getAsteroids fname
    let station = Pt 20 20
        antistation = Pt (-20) (-20)
        targets = asteroids & (filter (/= station)
                               >>> map (translate station)
                               >>> sortBy pointOrder
                               -- >>> sortBy simpleCompare
                               )
        partitioned = groupBy ((==) `on` getSlope) targets
        targetingOrder = roundRobin partitioned
        -- p@(Pt x y) = translate antistation (targetingOrder !! 199)
        -- Hack !!
        p@(Pt x y) = translate antistation $ (drop 117 targetingOrder ++ take 117 targetingOrder) !! 199
    putStrLn $ show p ++ " " ++ show (x * 100 + y)
    putStrLn "------ Sorted targets"
    let slope2rat VerticalUp = "inf"
        slope2rat VerticalDown = "-inf"
        slope2rat (Slope dy dx) = show (dy % dx)
        h (Pt x  y) = let h' = atan2 (negate y') x' + pi/2
                      in h' + if h' < 0 then pi else 0
           where conv = fromRational . fromIntegral
                 y' = conv y
                 x' = conv x
        round' x = (fromIntegral $ round (x * 100)) / 100
        info (i, p) = putStrLn $ show i ++ ") " ++ show (translate antistation p) ++ " " ++
                                 show (getQuadrant p) ++ " " ++
                                 -- slope2rat (getSlope p) ++ " " ++
                                 show (round' (getHeading p))
    traverse info (zip [0..] targets)
    putStrLn "------ Targeting order"
    traverse info (zip [0..] targetingOrder)
    putStrLn "------ Target partitioning"
    traverse print partitioned
    -- 489 too high
    -- not -95, -914, -1406, -1298, -209, -1906, -1212, 808, 607, 1310, 1719, 816
