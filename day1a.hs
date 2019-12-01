import System.IO
import System.Environment

fuelReq :: Int -> Int
fuelReq m = m `div` 3  - 2


realFuelReq :: Int -> Int
realFuelReq m = let fuelMass = fuelReq m
                    fuelMassReq = fuelReq fuelMass
                in if fuelMassReq <= 0 then fuelMass else (fuelMass + realFuelReq fuelMass)



main = do
    masses <- lines <$> readFile "day1a.input"
    a <- getArgs
    let f = case a of
              ["a"] -> fuelReq
              ["b"] -> realFuelReq
              otherwise -> error "'a' or 'b'"
    print (sum $ map f $ map read $ masses)
