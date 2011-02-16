module HaskellMD.Atom
( Element (..)
, toElement
, AtomID
, Charge
, AtomName
, Atom(..)
, toAtom
) where

import HaskellMD.Vector
import Data.List

data Element = H | Li | B | C | N | O | F | Na | Mg | Si | P | S | Cl | K | Ca | Br | I | Cs | Ba
        deriving (Show, Eq, Ord)

type AtomID = Integer
type Charge = Double
type AtomName = String

data Atom = Atom {
  name :: AtomName,
  element :: Element,
  charge :: Charge,
  atom_id :: AtomID
} deriving (Show, Eq, Ord)

toAtom :: (String, Element, Charge, AtomID) -> Atom
toAtom (n, e, c, i) = Atom {name=n, element=e, charge=c, atom_id=i}

toElement :: String -> Element
toElement s
  | "H" `isPrefixOf` s = H
  | "Li" `isPrefixOf` s = Li
  | "Ba" `isPrefixOf` s = Ba
  | "Br" `isPrefixOf` s = Br
  | "B" `isPrefixOf` s = B
  | "Cl" `isPrefixOf` s = Cl
  | "Ca" `isPrefixOf` s = Ca
  | "Cs" `isPrefixOf` s = Cs
  | "C" `isPrefixOf` s = C
  | "Na" `isPrefixOf` s = Na
  | "N" `isPrefixOf` s = N
  | "O" `isPrefixOf` s = O
  | "F" `isPrefixOf` s = F
  | "Mg" `isPrefixOf` s = Mg
  | "Si" `isPrefixOf` s = Si
  | "S" `isPrefixOf` s = S
  | "P" `isPrefixOf` s = P
  | "K" `isPrefixOf` s = K
  | "I" `isPrefixOf` s = I
