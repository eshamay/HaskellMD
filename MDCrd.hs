module HaskellMD.MDCrd
(readMDCrdFile) where

import Data.List.Split
import HaskellMD.Vector

readMDCrdFile :: String -> [Position]
readMDCrdFile input = 
        let     allwords = map words $ lines input
                numbers = map read $ concat allwords :: [Double]
                coords = splitEvery 3 numbers
                vectors = map (uncurry' vecr3d) coords
        in vectors
        
uncurry' :: (a -> a -> a -> b) -> [a] -> b
uncurry' f (x:y:z:zs) = f x y z
