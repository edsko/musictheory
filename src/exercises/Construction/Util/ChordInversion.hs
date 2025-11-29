-- | Utilities for working with chord inversions
--
-- Intended for qualified import.
--
-- > import Construction.Util.ChordInversion (ChordInversion(..))
-- > import Construction.Util.ChordInversion qualified as ChordInversion
module Construction.Util.ChordInversion (
    ChordInversion(..)
  , apply
  ) where

import MusicTheory

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ChordInversion = ChordInversion{
      inversion   :: Inversion
    , octaveShift :: OctaveShift
    , annotation  :: Ly.Annotation
    }

apply ::
     (TransposeOctave c, Invert c)
  => ChordInversion -> c -> c
apply i = transposeOctave i.octaveShift . invert i.inversion