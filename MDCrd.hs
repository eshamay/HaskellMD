module HaskellMD.MDCrd
(MDCrd (..),
 loadMDCrd,
 mdcrdLoadNext
 --mdcrdSkipNext,
 ) where

import Data.List.Split
import HaskellMD.Vector


type NumberOfAtoms = Int
data MDCrd = MDCrd { coordinates :: Coordinates,
                     systemSize :: Position,
                     fileData :: Coordinates,
                     numberOfAtoms :: NumberOfAtoms } 

-- Read the raw text data and updates the mutable container of vectors
loadMDCrd :: String -> NumberOfAtoms -> MDCrd
loadMDCrd dataFile num = 
  let allwords = map words $ lines dataFile :: [[String]]
      numbers = map read $ concat allwords :: [Double]
      coords = splitEvery 3 numbers :: [[Double]]
      vectors = map (uncurry' vecr3d) coords :: [Position]
  in MDCrd { coordinates = take num vectors :: [Position],
             systemSize = head $ drop num vectors,
             fileData = drop (num + 1) vectors,
             numberOfAtoms = num }

        
-- Drops the given number of coordinates in order to load up the next frame. This returns the updated list of coordinates, and the box vector that defines the box size
mdcrdLoadNext :: MDCrd -> MDCrd
mdcrdLoadNext mdcrd = 
  let num = numberOfAtoms mdcrd
      newData = drop num (fileData mdcrd)
      coords = take num (fileData mdcrd)
  in MDCrd {  coordinates = coords,
              systemSize = head newData,
              fileData = drop 1 newData,
              numberOfAtoms = num
            }


{-
-- skip a given number of timesteps, loading a given number of atoms in the mdcrd file
mdcrdSkipNext :: Integer -> NumberOfAtoms -> Coordinates -> (Coordinates,Position)
mdcrdSkipNext 1 atoms coords = mdcrdLoadNext atoms coords
mdcrdSkipNext i atoms coords = mdcrdSkipNext (i-1) atoms (fst $ mdcrdLoadNext atoms coords)
-}

uncurry' :: (a -> a -> a -> b) -> [a] -> b
uncurry' f (x:y:z:zs) = f x y z
