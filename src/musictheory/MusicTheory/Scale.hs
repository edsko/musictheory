-- | Scales
--
-- Intended for qualified import.
--
-- > import MusicTheory.Scale (Scale(..))
-- > import MusicTheory.Scale qualified as Scale
module MusicTheory.Scale (
    Scale(..)
  , at
    -- * Scale degrees
  , Degree(..)
  , firstDegree
    -- * Roots
  , Root(..)
  , rootNote
    -- * Scale names
  , Type(..)
  , Name(..)
    -- * Construction
  , named
    -- * Enumeration
  , allMajorRoots
  , allMinorRoots
  , enharmonicMajorRoots
  , enharmonicMinorRoots
  , allMajorScales
  , allMinorScales
    -- * Common scales
  , cMajor
  , cMinor
  , aMinor
  ) where

import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Util.StringTable

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Scale = Scale{
      name  :: Name
    , notes :: [Note]
    }
  deriving stock (Show)

at :: Scale -> Degree -> Note
at scale (Degree degree atal) =
    Note.withAccidental
      (scale.notes !! ((fromIntegral degree - 1) `mod` length scale.notes))
      atal

{-------------------------------------------------------------------------------
  Scale degrees
-------------------------------------------------------------------------------}

-- | Scale degrees
data Degree = Degree Word (Maybe Note.Accidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Degree

firstDegree :: Degree
firstDegree = Degree 1 Nothing

instance HasStringTable Degree where
  stringTable = uncurry (flip Degree) <$>
      stringTablePair
        (stringTableMaybe "" stringTable)
        (stringTableNum [1..14])

{-------------------------------------------------------------------------------
  Roots
-------------------------------------------------------------------------------}

-- | Scale roots along the circle of fifths
--
-- This includes some enharmonic scales (
data Root =
    C
  | G
  | D
  | A
  | E
  | B
  | Gb | F#
  | Db | C#
  | Ab | G#
  | Eb
  | Bb
  | F
  deriving stock (Eq, Enum, Bounded)
  deriving (Show, IsString) via UseStringTable Root

instance HasStringTable Root where
  stringTable = stringTableEnum $ stringTableEntry . rootNote

rootNote :: Root -> Note
rootNote = \case
    C  -> "C"
    G  -> "G"
    D  -> "D"
    A  -> "A"
    E  -> "E"
    B  -> "B"
    Gb -> "G♭" ; F# -> "F♯"
    Db -> "D♭" ; C# -> "C♯"
    Ab -> "A♭" ; G# -> "G♯"
    Eb -> "E♭"
    Bb -> "B♭"
    F  -> "F"

{-------------------------------------------------------------------------------
  Scale names
-------------------------------------------------------------------------------}

data Type =
    Major
  | Minor
  deriving stock (Show)

data Name = Name{
      root :: Root
    , typ  :: Type
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

named :: Name -> Scale
named (Name root typ) = Scale (Name root typ) $
    case typ of
      Major -> majorScale   root
      Minor -> naturalMinor root

majorScale :: Root -> [Note]
majorScale = \case
    C  -> ["C"  , "D"  , "E"  , "F"  , "G"  , "A"  , "B" ]
    G  -> ["G"  , "A"  , "B"  , "C"  , "D"  , "E"  , "F♯"]
    D  -> ["D"  , "E"  , "F♯" , "G"  , "A"  , "B"  , "C♯"]
    A  -> ["A"  , "B"  , "C♯" , "D"  , "E"  , "F♯" , "G♯"]
    E  -> ["E"  , "F♯" , "G♯" , "A"  , "B"  , "C♯" , "D♯"]
    B  -> ["B"  , "C♯" , "D♯" , "E"  , "F♯" , "G♯" , "A♯"]

    Gb -> ["G♭" , "A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F" ]
    F# -> ["F♯" , "G♯" , "A♯" , "B"  , "C♯" , "D♯" , "E♯"]

    Db -> ["D♭" , "E♭" , "F"  , "G♭" , "A♭" , "B♭" , "C" ]
    C# -> ["C♯" , "D♯" , "E♯" , "F♯" , "G♯" , "A♯" , "B♯"]

    Ab -> ["A♭" , "B♭" , "C"  , "D♭" , "E♭" , "F"  , "G" ]
    G# -> ["G♯" , "A♯" , "B♯" , "C♯" , "D♯" , "E♯" , "F𝄪" ]

    Eb -> ["E♭" , "F"  , "G"  , "A♭" , "B♭" , "C"  , "D" ]
    Bb -> ["B♭" , "C"  , "D"  , "E♭" , "F"  , "G"  , "A" ]
    F  -> ["F"  , "G"  , "A"  , "B♭" , "C"  , "D"  , "E" ]

naturalMinor :: Root -> [Note]
naturalMinor = \case
    C  -> ["C"  , "D"  , "E♭" , "F"  , "G"  , "A♭" , "B♭"]
    G  -> ["G"  , "A"  , "B♭" , "C"  , "D"  , "E♭" , "F" ]
    D  -> ["D"  , "E"  , "F"  , "G"  , "A"  , "B♭" , "C" ]
    A  -> ["A"  , "B"  , "C"  , "D"  , "E"  , "F"  , "G" ]
    E  -> ["E"  , "F♯" , "G"  , "A"  , "B"  , "C"  , "D" ]
    B  -> ["B"  , "C♯" , "D"  , "E"  , "F♯" , "G"  , "A" ]

    Gb -> ["G♭" , "A♭" , "B𝄫" , "C♭" , "D♭" , "E𝄫" , "F♭"]
    F# -> ["F♯" , "G♯" , "A"  , "B"  , "C♯" , "D"  , "E" ]

    Db -> ["D♭" , "E♭" , "F♭" , "G♭" , "A♭" , "B𝄫" , "C♭"]
    C# -> ["C♯" , "D♯" , "E"  , "F♯" , "G♯" , "A"  , "B" ]

    Ab -> ["A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F♭" , "G♭"]
    G# -> ["G♯" , "A♯" , "B"  , "C♯" , "D♯" , "E"  , "F♯"]

    Eb -> ["E♭" , "F"  , "G♭" , "A♭" , "B♭" , "C♭" , "D♭"]
    Bb -> ["B♭" , "C"  , "D♭" , "E♭" , "F"  , "G♭" , "A♭"]
    F  -> ["F"  , "G"  , "A♭" , "B♭" , "C"  , "D♭" , "E♭"]

{-------------------------------------------------------------------------------
  Choice of roots

  For the minor scale we pick roots so that double-flats are avoided. For a
  sense of duality, we then pick their enharmonic equivalents for major scales.

  For the minor scales, we start the circle of fifths on A, to get a similar
  progression in terms of sharps and flats as for the major scales.
-------------------------------------------------------------------------------}

allMinorRoots, allMajorRoots :: [Root]
allMajorRoots = [C, G, D, A, E, B,   Gb, Db, Ab,   Eb, Bb, F]
allMinorRoots = [C, G, D, A, E, B,   F#, C#, G#,   Eb, Bb, F]

enharmonicMajorRoots, enharmonicMinorRoots :: [Root]
enharmonicMajorRoots = [F#, C#] -- we omit G#
enharmonicMinorRoots = [Gb, Db, Ab]

allMajorScales, allMinorScales :: [Scale]
allMajorScales        = [named (Name root Major) | root <- allMajorRoots]
allMinorScales        = [named (Name root Minor) | root <- allMinorRoots]

{-------------------------------------------------------------------------------
  Common scales
-------------------------------------------------------------------------------}

cMajor, cMinor, aMinor :: Scale
cMajor = named $ Name C Major
cMinor = named $ Name C Minor
aMinor = named $ Name A Minor
