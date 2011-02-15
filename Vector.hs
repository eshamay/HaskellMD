module HaskellMD.Vector
(Position,
 vecr3d,
 cosAngle) where

import Data.Packed.Vector
import Numeric.Container

type Position = Vector Double
type Coordinates = [Position]

vecr3d :: Double -> Double -> Double -> Position
vecr3d a b c = fromList [a,b,c] :: Position

cosAngle :: Position -> Position -> Double
cosAngle a b = (dot a b) / (norm2 a) / (norm2 b)

