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

    let doit :: (Int -> Int) -> IO ()
        doit f = print $ sum $ (map f masses)

    case a of
      ["a"] -> doit fuelReq
      ["b"] -> doit realFuelReq
      otherwise -> die "One argument 'a' or 'b' is required"
