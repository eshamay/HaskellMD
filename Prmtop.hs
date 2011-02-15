module HaskellMD.Prmtop
(readPrmtopFile
 ) where

import qualified Data.Map as Map
import Data.List
import Data.List.Split

readPrmtopFile :: String -> [(String,[String])]
readPrmtopFile input = 
        let     alllines = snd $ break isFlag $ lines input
                splitSections input =
                        (fst $ s) : fst $ break isFlag s
                            where s = span isFlag input
        in sections

isFlag :: String -> Bool
isFlag x = isPrefixOf "%FLAG" x

