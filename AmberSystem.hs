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

-- How to do mutable data from an incoming data file:
  -- Prelude> :m Data.IORef 
  -- Prelude Data.IORef> x <- newIORef [] :: IO (IORef [Int]) 
  -- Prelude Data.IORef> writeIORef x [1,2,3] 
  -- Prelude Data.IORef> readIORef x 
  -- [1,2,3] 
import Data.IORef

-- the full amber system from which lists of atoms can be parsed
type PrmtopFile = FilePath
type MDCrdFile = FilePath

data AmberSystem = AmberSystem Prmtop (IO (IORef [Position]))

loadAmberSystem :: PrmtopFile -> MDCrdFile -> IO AmberSystem
loadAmberSystem prmtoppath mdcrdpath = do
  prmtop <- readFile prmtoppath
  mdcrd <- readFile mdcrdpath
  coordinates <- newIORef [] :: IO (IORef [Position])
  AmberSystem (parsePrmtopFile prmtop) (parseMDCrdFile mdcrd coordinates)

{-
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
-}
