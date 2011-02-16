module HaskellMD.AmberSystem
(loadAmberSystem,
 toAtomList,
 toCoordinates, 
 loadNext,
 skipNext,
 ) where

import Data.List
import HaskellMD.Vector
import HaskellMD.Atom
import HaskellMD.Prmtop
import HaskellMD.MDCrd

-- the full amber system from which lists of atoms can be parsed
data AmberSystem = AmberSystem Prmtop Coordinates
type PrmtopFile = FilePath
type MDCrdFile = FilePath

loadAmberSystem :: PrmtopFile -> MDCrdFile -> IO AmberSystem
loadAmberSystem prmtoppath mdcrdpath = do
  prmtop <- readFile prmtoppath
  mdcrd <- readFile mdcrdpath
  return $ AmberSystem (parsePrmtopFile prmtop) (parseMDCrdFile mdcrd)

type AtomList = [(Atom,Position)]

-- extract the atom list from the amber system
toAtomList :: AmberSystem -> [Atom]
toAtomList (AmberSystem prmtop mdcrd) = 
  let 
    names = atom_name prmtop
    atom_data = zip4 names (map toElement names) (HaskellMD.Prmtop.charge prmtop) [0..]
  in map toAtom atom_data

toCoordinates :: AmberSystem -> Coordinates
toCoordinates (AmberSystem prmtop mdcrd) = mdcrd

-- load the next timestep
loadNext :: AmberSystem -> AmberSystem
loadNext (AmberSystem prmtop mdcrd) = 
  AmberSystem prmtop (fst $ mdcrdLoadNext (numAtoms prmtop) mdcrd)

-- skip a number of timesteps
skipNext :: Integer -> AmberSystem -> AmberSystem
skipNext n (AmberSystem prmtop mdcrd) =
  AmberSystem prmtop (fst $ mdcrdSkipNext n (numAtoms prmtop) mdcrd)
