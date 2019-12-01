import System.IO

fuelReq :: Int -> Int
fuelReq m = m `div` 3  - 2



main = do
    contents <- readFile "day1a.input"
    print (sum $ map fuelReq $ map read $ lines contents)
