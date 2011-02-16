module HaskellMD.MDCrd
(parseMDCrdFile,
 mdcrdLoadNext,
 mdcrdSkipNext,
 ) where

import Data.List.Split
import HaskellMD.Vector

type RawTextData = String

parseMDCrdFile :: RawTextData -> Coordinates
parseMDCrdFile input = 
        let     allwords = map words $ lines input
                numbers = map read $ concat allwords :: [Double]
                coordinates = splitEvery 3 numbers
                vectors = map (uncurry' vecr3d) coordinates
        in vectors
        
type NumberOfAtoms = Integer
-- Drops the given number of coordinates in order to load up the next frame. This returns the updated list of coordinates, and the box vector that defines the box size
mdcrdLoadNext :: NumberOfAtoms -> Coordinates -> (Coordinates,Position)
mdcrdLoadNext i coords = let
	new_coords = drop (fromInteger (i)) coords
	in (tail new_coords, head new_coords)

-- skip a given number of timesteps, loading a given number of atoms in the mdcrd file
mdcrdSkipNext :: Integer -> NumberOfAtoms -> Coordinates -> (Coordinates,Position)
mdcrdSkipNext 1 atoms coords = mdcrdLoadNext atoms coords
mdcrdSkipNext i atoms coords = mdcrdSkipNext (i-1) atoms (fst $ mdcrdLoadNext atoms coords)

uncurry' :: (a -> a -> a -> b) -> [a] -> b
uncurry' f (x:y:z:zs) = f x y z
