module HaskellMD.Prmtop
(readPrmtopFile
 ) where

import qualified Data.Map as Map
import Data.List
import Data.List.Split

readPrmtopFile :: String -> [[String]]
readPrmtopFile input = 
        let     alllines = lines input :: [String]
                sections = splitWhen (not . isFlag) alllines
        in sections

isFlag :: String -> Bool
isFlag x = isPrefixOf "%FLAG" x
