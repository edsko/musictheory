-- | Scales
--
-- Intended for qualified import.
--
-- > import MusicTheory.Scale (Scale)
-- > import MusicTheory.Scale qualified as Scale
module MusicTheory.Scale (
    -- * Basic definitions
    Name(..)
  , rootNote
  , scaleNames
  , Scale(..)
    -- * Standard scales
  , majorScale
    -- * Operations that only make sense in the context of a specific scale
  , InScale -- opaque
  , inScale
  , wrtScale
    -- * Scale degrees
  , Degree(..)
  , fromDegree
  ) where

import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Data.Functor.Identity
import Data.List ((!?))
import GHC.Stack

import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Util

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

-- | Scale names as used in the circle of fifths
data Name = C | G | D | A | E | B | F# | Gb | Db | Ab | Eb | Bb | F
  deriving stock (Eq, Enum, Bounded)
  deriving (Show, IsString) via UseStringTable Name

rootNote :: Name -> Note.Simple
rootNote = \case
    C  -> "C"
    G  -> "G"
    D  -> "D"
    A  -> "A"
    E  -> "E"
    B  -> "B"
    F# -> "F♯"
    Gb -> "G♭"
    Db -> "D♭"
    Ab -> "A♭"
    Eb -> "E♭"
    Bb -> "B♭"
    F  -> "F"

instance HasStringTable Name where
  stringTable = stringTableEnum $ stringTableLookup stringTable . rootNote

scaleNames :: [Name]
scaleNames = [minBound .. maxBound]

data Scale = Scale{
      notes :: [Note.Simple]
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Major scales around the circle of fifths

  Scales /determine/ the context for determining note names, so in order to
  construct these scales we use normalized notes, and then superimpose the
  standard conventions.
-------------------------------------------------------------------------------}

majorScaleNorm :: Name -> [Note.Norm]
majorScaleNorm = \case
    C     -> map Note.normalize [Note.C .. Note.B]
    Gb    -> majorScaleNorm F#
    scale -> Note.transpose 7 (majorScaleNorm (pred scale))

defaultAccidentals :: HasCallStack => Name -> Note.SimpleAccidental
defaultAccidentals = \case
    C  -> error "No default accidental in C"
    G  -> "♯"
    D  -> "♯"
    A  -> "♯"
    E  -> "♯"
    B  -> "♯"
    F# -> "♯"
    Gb -> "♭"
    Db -> "♭"
    Ab -> "♭"
    Eb -> "♭"
    Bb -> "♭"
    F  -> "♭"

majorScale :: Name -> Scale
majorScale name = Scale $
    map (Note.fromNorm (defaultAccidentals name)) $
      majorScaleNorm name

{-------------------------------------------------------------------------------
  Operations that only make sense in the context of a specific scale
-------------------------------------------------------------------------------}

newtype InScale a = WrapInScale{
      unwrapInScale :: Reader Scale a
    }

inScale :: (Scale -> a) -> InScale a
inScale f = WrapInScale $ Reader.ReaderT $ Identity . f

wrtScale :: Scale -> InScale a -> a
wrtScale scale = flip Reader.runReader scale . unwrapInScale

{-------------------------------------------------------------------------------
  Scale degrees
-------------------------------------------------------------------------------}

-- | Scale degrees
--
-- NOTE: We use programmer's convention here, not musical convention: we count
-- from /zero/, not /one/.
data Degree = Degree Word (Maybe Note.SimpleAccidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Degree

instance HasStringTable Degree where
  stringTable = uncurry Degree <$>
      stringTablePair stringTableDegree (stringTableMaybe "" stringTable)

-- | String table for scale degrees
--
-- We use uppercase Roman numerals. This is somewhat non-standard; typically
-- uppercase is used for major chords and lowercase for minor chords, but here
-- we are not necessarily talking about chords, but simply about individual
-- notes in a scale.
stringTableDegree :: StringTable Word
stringTableDegree = StringTable [
      (0, "I")
    , (1, "II")
    , (2, "III")
    , (3, "IV")
    , (4, "V")
    , (5, "VI")
    , (6, "VII")
    ]

fromDegree :: Degree -> InScale Note
fromDegree (Degree degree atal) = inScale $ \scale ->
    case scale.notes !? fromIntegral degree of
      Just note -> Note.simpleWithAccidental note atal
      Nothing   -> error $ concat [
          "Invalid scale degree " ++ show degree
        , " in scale " ++ show scale
        ]
