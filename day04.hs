import Data.List.Split
import Data.List

input :: String
input = "145852-616942"

generateOrderings :: [Char] -> [Ordering]
generateOrderings = map (uncurry compare) . (zip <*> tail)

hasEQ :: [Ordering] -> Bool
hasEQ = elem EQ

hasNoGT :: [Ordering] -> Bool
hasNoGT = notElem GT

hasLoneEQ :: [Ordering] -> Bool
hasLoneEQ = elem [EQ] . group

validate :: [Char] -> Bool
validate l@[_,_,_,_,_,_] = let x = generateOrderings l
                           in hasEQ x && hasNoGT x
validate _ = False

betterValidate :: [Char] -> Bool
betterValidate l@[_,_,_,_,_,_] = let x = generateOrderings l
                                 in hasLoneEQ x && hasNoGT x
                      

processInput :: String -> [Int]
processInput = map read . splitOn "-"

main = do
  let low, high::Int
      [low, high] = processInput input
  print $ length $ filter (validate . show ) [low..high]  -- 1767
  print $ length $ filter (betterValidate . show ) [low..high]  -- 1192
