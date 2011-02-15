module HaskellMD.Prmtop
(Prmtop (..),
 parsePrmtopFile
 ) where

import Data.List
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe

data Prmtop = Prmtop 
  { pointers :: [Integer],
    residue_pointers :: [Integer],
    atom_name :: [String],
    residue_label :: [String],
    charge :: [Double],
    mass :: [Double]
  } deriving (Show)

parsePrmtopSection = (map read . words . concat . fromJust)

--ParsePrmtopFile :: String -> Prmtop
parsePrmtopFile input = let 
    prmtop = readPrmtopFile input
    record = Prmtop { 
        atom_name = words $ concat $ fromJust $ Map.lookup "ATOM_NAME" prmtop,
        residue_label = words $ concat $ fromJust $ Map.lookup "RESIDUE_LABEL" prmtop,
        charge = (map read $ words $ concat $ fromJust $ Map.lookup "CHARGE" prmtop) :: [Double],
        mass = (map read $ words $ concat $ fromJust $ Map.lookup "MASS" prmtop) :: [Double],
        pointers = (parsePrmtopSection $ Map.lookup "POINTERS" prmtop) :: [Integer],
        residue_pointers = (parsePrmtopSection $ Map.lookup "RESIDUE_POINTER" prmtop) :: [Integer]
    }
  in record

-- parse a prmtop file and return a map
readPrmtopFile :: String -> Map.Map String [String]
readPrmtopFile input = 
        let alllines = snd $ break isFlag $ lines input
        in Map.fromList $ parseSections alllines


-- does the deconstruction of the various sections of the file
parseSections :: [String] -> [(String,[String])]
parseSections [] = []
-- this part forms the tuples for the map, and also removes all the FORMAT specifiers for each section
parseSections (x:xs) = (parseFlag x, tail $ fst s) : (parseSections $ snd s)
	where s = break isFlag xs


-- parses the name of the FLAG identifiers
parseFlag :: String -> String
parseFlag x = (trim . snd) $ break (\y -> y == ' ') x


-- predicate for checking if a line is a flag
-- lines starting new sections are flags, and always have the form "%FLAG <flagname>"
isFlag :: String -> Bool
isFlag x = isPrefixOf "%FLAG " x

-- removes whitespace from strings
trim      :: String -> String
trim      = f . f
	where f = reverse . dropWhile isSpace

