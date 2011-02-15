import System
import Data.List.Split
import HaskellMD.Vector
import HaskellMD.MDCrd
import HaskellMD.Prmtop

main = do 
        prmtop <- getContents
        putStr $ (unlines . concat . readPrmtopFile) prmtop
