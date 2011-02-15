module HaskellMD.AmberSystem
(AmberSystem)
    where

import HaskellMD.Atom
import HaskellMD.Vector

data AmberSystem = {
  prmtop :: Prmtop,
  mdcrd :: [Position]
}

type prmtopFile = String
type mdcrdFile = String

loadAmberSystem :: prmtopFile -> mdcrdFile -> [Atom]
loadAmberSystem prmtop mdcrd = let 
    names = atom_name prmtop
    atom_data = zip5 names (map toElement names) mdcrd (charge prmtop) [0..]
  in map toAtom atom_data
