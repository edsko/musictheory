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
    -- * Scale degrees
  , Degree(..)
  , fromDegree
  ) where

import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Util.StringTable

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

  Scales /determine/ the context for determining note names, so we construct
  these by hand.
-------------------------------------------------------------------------------}

majorScale :: Name -> Scale
majorScale = Scale . \case
    C  -> ["C"  , "D"  , "E"  , "F"  , "G"  , "A"  , "B" ]
    G  -> ["G"  , "A"  , "B"  , "C"  , "D"  , "E"  , "F♯"]
    D  -> ["D"  , "E"  , "F♯" , "G"  , "A"  , "B"  , "C♯"]
    A  -> ["A"  , "B"  , "C♯" , "D"  , "E"  , "F♯" , "G♯"]
    E  -> ["E"  , "F♯" , "G♯" , "A"  , "B"  , "C♯" , "D♯"]
    B  -> ["B"  , "C♯" , "D♯" , "E"  , "F♯" , "G♯" , "A♯"]
    F# -> ["F♯" , "G♯" , "A♯" , "B"  , "C♯" , "D♯" , "E♯"]
    Gb -> ["G♭" , "A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F" ]
    Db -> ["D♭" , "E♭" , "F"  , "G♭" , "A♭" , "B♭" , "C" ]
    Ab -> ["A♭" , "B♭" , "C"  , "D♭" , "E♭" , "F"  , "G" ]
    Eb -> ["E♭" , "F"  , "G"  , "A♭" , "B♭" , "C"  , "D" ]
    Bb -> ["B♭" , "C"  , "D"  , "E♭" , "F"  , "G"  , "A" ]
    F  -> ["F"  , "G"  , "A"  , "B♭" , "C"  , "D"  , "E" ]

{-------------------------------------------------------------------------------
  Scale degrees
-------------------------------------------------------------------------------}

-- | Scale degrees
data Degree = Degree Word (Maybe Note.SimpleAccidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Degree

instance HasStringTable Degree where
  stringTable = uncurry (flip Degree) <$>
      stringTablePair (stringTableMaybe "" stringTable) stringTableDegree

-- | String table for scale degrees
--
-- We use uppercase Roman numerals. This is somewhat non-standard; typically
-- uppercase is used for major chords and lowercase for minor chords, but here
-- we are not necessarily talking about chords, but simply about individual
-- notes in a scale.
stringTableDegree :: StringTable Word
stringTableDegree = StringTable [
      ( 1,  "1")
    , ( 2,  "2")
    , ( 3,  "3")
    , ( 4,  "4")
    , ( 5,  "5")
    , ( 6,  "6")
    , ( 7,  "7")
    , ( 8,  "8")
    , ( 9,  "9")
    , (10, "10")
    , (11, "11")
    , (12, "12")
    , (13, "13")
    , (14, "14")
    ]

fromDegree :: Scale -> Degree -> Note
fromDegree Scale{notes = scale} (Degree degree atal) =
    Note.simpleWithAccidental
      (scale !! ((fromIntegral degree - 1) `mod` length scale))
      atal
