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
  , allMajorScales
  , allMinorScales
  ) where

import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Util.StringTable

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Scale = Scale{
      name  :: Name
    , notes :: [Note.Simple]
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
data Degree = Degree Word (Maybe Note.SimpleAccidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Degree

firstDegree :: Degree
firstDegree = Degree 1 Nothing

instance HasStringTable Degree where
  stringTable = uncurry (flip Degree) <$>
      stringTablePair (stringTableMaybe "" stringTable) stringTableDegree
    where
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

{-------------------------------------------------------------------------------
  Roots
-------------------------------------------------------------------------------}

-- | Scale roots along the circle of fifths
data Root =
    C
  | G
  | D
  | A
  | E
  | B
  | F# | Gb
  | Db | C#
  | Ab
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

    F# -> "F♯"
    Gb -> "G♭"

    Db -> "D♭"
    C# -> "C♯"

    Ab -> "A♭"
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

majorScale :: Root -> [Note.Simple]
majorScale = \case
    C  -> ["C"  , "D"  , "E"  , "F"  , "G"  , "A"  , "B" ]
    G  -> ["G"  , "A"  , "B"  , "C"  , "D"  , "E"  , "F♯"]
    D  -> ["D"  , "E"  , "F♯" , "G"  , "A"  , "B"  , "C♯"]
    A  -> ["A"  , "B"  , "C♯" , "D"  , "E"  , "F♯" , "G♯"]
    E  -> ["E"  , "F♯" , "G♯" , "A"  , "B"  , "C♯" , "D♯"]
    B  -> ["B"  , "C♯" , "D♯" , "E"  , "F♯" , "G♯" , "A♯"]

    F# -> error "use Gb instead"
    Gb -> ["G♭" , "A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F" ]

    Db -> ["D♭" , "E♭" , "F"  , "G♭" , "A♭" , "B♭" , "C" ]
    C# -> error "use Db instead"

    Ab -> ["A♭" , "B♭" , "C"  , "D♭" , "E♭" , "F"  , "G" ]
    Eb -> ["E♭" , "F"  , "G"  , "A♭" , "B♭" , "C"  , "D" ]
    Bb -> ["B♭" , "C"  , "D"  , "E♭" , "F"  , "G"  , "A" ]
    F  -> ["F"  , "G"  , "A"  , "B♭" , "C"  , "D"  , "E" ]

-- | Natural minor scale
--
-- We do not provide values for Gb and Db; these scales have double flats:
--
-- > Gb -> ["G♭" , "A♭" , "B𝄫" , "C♭" , "D♭" , "E𝄫" , "F♭"]
-- > Db -> ["D♭" , "E♭" , "F♭" , "G♭" , "A♭" , "B𝄫" , "C♭"]
naturalMinor :: Root -> [Note.Simple]
naturalMinor = \case
    C  -> ["C"  , "D"  , "E♭" , "F"  , "G"  , "A♭" , "B♭"]
    G  -> ["G"  , "A"  , "B♭" , "C"  , "D"  , "E♭" , "F" ]
    D  -> ["D"  , "E"  , "F"  , "G"  , "A"  , "B♭" , "C" ]
    A  -> ["A"  , "B"  , "C"  , "D"  , "E"  , "F"  , "G" ]
    E  -> ["E"  , "F♯" , "G"  , "A"  , "B"  , "C"  , "D" ]
    B  -> ["B"  , "C♯" , "D"  , "E"  , "F♯" , "G"  , "A" ]

    F# -> ["F♯" , "G♯" , "A"  , "B"  , "C♯" , "D"  , "E" ]
    Gb -> error "use F♯ instead"

    Db -> error "use C♯ instead"
    C# -> ["C♯" , "D♯" , "E"  , "F♯" , "G♯" , "A"  , "B" ]

    Ab -> ["A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F♭" , "G♭"]
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
allMajorRoots = [C, G, D, A, E, B,   Gb, Db,   Ab, Eb, Bb, F]
allMinorRoots = [         A, E, B,   F#, C#,   Ab, Eb, Bb, F, C, G, D]

allMajorScales, allMinorScales :: [Scale]
allMajorScales = [named (Name root Major) | root <- allMajorRoots]
allMinorScales = [named (Name root Minor) | root <- allMinorRoots]
