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
  , MakeAbsolute(..)
  , absoluteReference
  ) where

import Data.Kind

import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ReferenceKind = Absolute | Relative

type family Reference (r :: ReferenceKind) where
  Reference Absolute = Note.InOctave
  Reference Relative = Scale.Degree

{-------------------------------------------------------------------------------
  Singleton
-------------------------------------------------------------------------------}

data SReferenceKind :: ReferenceKind -> Type where
  SAbsoluteKind :: SReferenceKind Absolute
  SRelativeKind :: SReferenceKind Relative

class ( Show (Reference r)
      ) => IsReferenceKind r where
  isReferenceKind :: SReferenceKind r

instance IsReferenceKind Absolute where isReferenceKind = SAbsoluteKind
instance IsReferenceKind Relative where isReferenceKind = SRelativeKind

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

class MakeAbsolute f where
   wrtScale :: Octave -> Scale -> f Relative -> f Absolute

-- | Translate relative references into absolute ones
--
-- This is essentially an instance of 'MakeAbsolute', but for a type family.
absoluteReference ::
     Octave
  -> Scale
  -> Reference Relative
  -> Reference Absolute
absoluteReference octave scale = Note.InOctave octave . Scale.at scale
