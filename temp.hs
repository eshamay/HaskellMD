import HaskellMD.Prmtop
import qualified Data.Map as Map

main = do 
	contents <- getContents
	(print . concat) $ Map.lookup "ATOM_NAME" $ readPrmtopFile contents
