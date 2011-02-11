module HaskellMD.Atom
( Element (H, Li, B, C, N, O, F, Na, Mg, Si, P, S, Cl, K, Ca, Br, I, Cs, Ba)
, AtomID
, Charge
, Atom(Atom)
) where

import HaskellMD.Vector

data Element = H | Li | B | C | N | O | F | Na | Mg | Si | P | S | Cl | K | Ca | Br | I | Cs | Ba
        deriving (Show, Eq, Ord)

type AtomID = Integer
type Charge = Double

data Atom = Atom {
  element :: Element,
  position :: Position,
  charge :: Charge,
  id :: AtomID
} deriving (Show, Eq, Ord)


