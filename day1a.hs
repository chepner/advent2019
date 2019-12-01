import System.IO

fuelReq :: Int -> Int
fuelReq m = m `div` 3  - 2


realFuelReq :: Int -> Int
realFuelReq m = let fuelMass = fuelReq m
                    fuelMassReq = fuelReq fuelMass
                in if fuelMassReq <= 0 then fuelMass else (fuelMass + realFuelReq fuelMass)



main = do
    contents <- readFile "day1a.input"
    print (sum $ map realFuelReq $ map read $ lines contents)
