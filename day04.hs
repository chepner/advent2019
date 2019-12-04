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

correctLength :: [Char] -> Bool
correctLength [_,_,_,_,_,_] = True
correctLength _ = False

validateWith :: [[Ordering]->Bool] -> [Char] -> Bool
validateWith ps l = correctLength l && and (map ($ generateOrderings l) ps)

validate :: [Char] -> Bool
validate = validateWith [hasEQ, hasNoGT]

betterValidate :: [Char] -> Bool
betterValidate = validateWith [hasLoneEQ, hasNoGT]
                      

processInput :: String -> [Int]
processInput = map read . splitOn "-"

main = do
  let [low, high] = processInput input
  print $ length $ filter (validate . show ) [low..high]  -- 1767
  print $ length $ filter (betterValidate . show ) [low..high]  -- 1192
