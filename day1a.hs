import System.IO
import System.Environment
import System.Exit

fuelReq :: Int -> Int
fuelReq m = m `div` 3  - 2


realFuelReq :: Int -> Int
realFuelReq m = let fuelMass = fuelReq m
                    fuelMassReq = fuelReq fuelMass
                in if fuelMassReq <= 0 then fuelMass else (fuelMass + realFuelReq fuelMass)



main = do
    masses <- (map read . lines) <$> readFile "day1a.input"
    
    a <- getArgs

    case a of
      ["a"] -> print (sum $ map fuelReq $ masses)
      ["b"] -> print (sum $ map realFuelReq $ masses)
      otherwise -> die "One argument 'a' or 'b' is required"
