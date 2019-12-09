import System.IO
import System.Environment
import Data.Char
import Data.List.Split
import Data.List
import Data.Ord

import Control.Monad.Trans.State

data Pixel = Black | White | Transparent deriving (Enum, Show)

-- The image consists of white letters on a black background,
-- but it's easier to read if I invert the pixels. 
pixelToChar :: Pixel -> Char
pixelToChar Black = ' '
pixelToChar White = '*'
pixelToChar Transparent = pixelToChar White -- Default to the (white) background

instance Semigroup Pixel where
  Transparent <> t = t
  t <> _ = t

type Layer = [Pixel]
data Image = Image Int Int Layer

getLayers :: Int -> [Pixel] -> [Layer]
getLayers s = chunksOf s

composeLayers :: [Layer] -> Layer
composeLayers = foldr1 (zipWith (<>))

getImage :: Int -> Int -> [Pixel] -> Image
getImage x y = Image x y . composeLayers . getLayers (x * y)

getData :: FilePath -> Int -> Int -> IO Image
getData fname x y = do
    contents <- readFile fname
    return $ getImage x y $ map (toEnum . digitToInt) (init contents)


displayImage :: Image -> IO ()
displayImage (Image x y layer) = let pixels = map pixelToChar layer 
                                     rows = chunksOf x pixels
                                     go [] = return ()
                                     go (r:rs) = putStrLn r >> go rs
                                 in  go rows

main = do
  [fname] <- getArgs
  image <- getData fname 25 6

  displayImage image  -- FKAHL
  
