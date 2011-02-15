import System
import Data.List.Split
import HaskellMD.Vector
import HaskellMD.MDCrd

main = do 
        mdcrd <- getContents
        putStr $ (show . head . readMDCrdFile) mdcrd

