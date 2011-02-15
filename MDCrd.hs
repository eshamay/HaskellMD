module HaskellMD.MDCrd
(readMDCrdFile) where

import Data.List.Split
import HaskellMD.Vector

type RawTextData = String

readMDCrdFile :: RawTextData -> Coordinates
readMDCrdFile input = 
        let     allwords = map words $ lines input
                numbers = map read $ concat allwords :: [Double]
                coordinates = splitEvery 3 numbers
                vectors = map (uncurry' vecr3d) coords
        in vectors
        
-- Drops the given number of coordinates in order to load up the next frame. This returns the updated list of coordinates, and the box vector that defines the box size
mdcrdLoadNext :: Int -> Coordinates -> (Coordinates,Vector)
mdcrdLoadNext i coords = let
	new_coords = drop i coords
	in (tail new_coords, head new_coords)

uncurry' :: (a -> a -> a -> b) -> [a] -> b
uncurry' f (x:y:z:zs) = f x y z
