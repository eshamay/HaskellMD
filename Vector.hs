
module HaskellMD.Vector
(vecr3d
) where

import Data.Packed.Vector
import Numeric.Container

--vecr3d :: Double -> Double -> Double -> Vector
vecr3d x y z = fromList [x,y,z] :: Vector Double

