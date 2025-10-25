-- | Absolute references vs relative reference (wrt to a choice of scale)
--
-- Intended for unqualified.
--
-- > import MusicTheory.Reference
module MusicTheory.Reference (
    ReferenceKind(..)
  , Reference
    -- * Singleton
  , SReferenceKind(..)
  , IsReferenceKind(..)
    -- * Translation
  , referenceWrtScale
  ) where

import Data.Kind

import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ReferenceKind = Abs | Rel

type family Reference (r :: ReferenceKind) where
  Reference Abs = Note.InOctave
  Reference Rel = Scale.Degree

{-------------------------------------------------------------------------------
  Singleton
-------------------------------------------------------------------------------}

data SReferenceKind :: ReferenceKind -> Type where
  SAbsoluteKind :: SReferenceKind Abs
  SRelativeKind :: SReferenceKind Rel

class ( Show (Reference r)
      , Eq   (Reference r)
      ) => IsReferenceKind r where
  isReferenceKind :: SReferenceKind r

instance IsReferenceKind Abs where isReferenceKind = SAbsoluteKind
instance IsReferenceKind Rel where isReferenceKind = SRelativeKind

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

-- | Translate relative references into absolute ones
referenceWrtScale :: Scale -> Octave -> Reference Rel -> Reference Abs
referenceWrtScale scale octave = Note.InOctave octave . Scale.at scale
