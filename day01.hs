import System.IO
import System.Environment
import System.Exit

type Mass = Int
type FuelCalculator = Mass -> Mass

fuelReq :: FuelCalculator
fuelReq m = m `div` 3  - 2


realFuelReq :: FuelCalculator
realFuelReq m = let fuelMass = fuelReq m
                    fuelMassReq = fuelReq fuelMass
                in if fuelMassReq <= 0 then fuelMass else (fuelMass + realFuelReq fuelMass)


readMasses :: FilePath -> IO [Mass]
readMasses fname = (map read . lines) <$> readFile fname


main = do
    masses <- readMasses "day01.input"
    
    a <- getArgs

    let doit :: FuelCalculator -> IO ()
        doit f = print $ sum $ (map f masses)

    case a of
      ["a"] -> doit fuelReq
      ["b"] -> doit realFuelReq
      otherwise -> die "One argument 'a' or 'b' is required"
