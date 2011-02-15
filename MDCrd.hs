module HaskellMD.MDCrd
(readMDCrdFile,
 mdcrdLoadNext,
 mdcrdSkipNext,
 ) where

import Data.List.Split
import HaskellMD.Vector

type RawTextData = String

readMDCrdFile :: RawTextData -> Coordinates
readMDCrdFile input = 
        let     allwords = map words $ lines input
                numbers = map read $ concat allwords :: [Double]
                coordinates = splitEvery 3 numbers
                vectors = map (uncurry' vecr3d) coordinates
        in vectors
        
-- Drops the given number of coordinates in order to load up the next frame. This returns the updated list of coordinates, and the box vector that defines the box size
type NumberAtoms = Int
mdcrdLoadNext :: NumberAtoms -> Coordinates -> (Coordinates,Position)
mdcrdLoadNext i coords = let
	new_coords = drop (3*i) coords
	in (tail new_coords, head new_coords)

-- skip a given number of timesteps in the mdcrd file
mdcrdSkipNext :: Int -> NumberAtoms -> Coordinates -> (Coordinates,Position)
mdcrdSkipNext 1 atoms coords = mdcrdLoadNext atoms coords
mdcrdSkipNext i atoms coords = mdcrdSkipNext (i-1) atoms (fst $ mdcrdLoadNext atoms coords)

uncurry' :: (a -> a -> a -> b) -> [a] -> b
uncurry' f (x:y:z:zs) = f x y z
