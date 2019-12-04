import Data.List.Split
import Data.List

input :: String
input = "145852-616942"

generateOrderings :: [Char] -> [Ordering]
generateOrderings = map (uncurry compare) . (zip <*> tail)

validate :: [Char] -> Bool
validate l@[_,_,_,_,_,_] = let x = generateOrderings l
                           in EQ `elem` x && GT `notElem` x
validate _ = False


properEQ :: [Ordering] -> Bool
properEQ xs = [EQ] `elem` group xs

betterValidate :: [Char] -> Bool
betterValidate l@[_,_,_,_,_,_] = let x = generateOrderings l
                                 in EQ `elem` x
                                    && GT `notElem` x
                                    && properEQ x
                      

processInput :: String -> [Int]
processInput = map read . splitOn "-"

main = do
  let low, high::Int
      [low, high] = processInput input
  --print $ length $ filter (validate . show ) [low..high]  -- 1767
  print $ length $ filter (betterValidate . show ) [low..high]  -- 1192
