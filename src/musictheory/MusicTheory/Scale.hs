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
  , allRoots
    -- * Scale names
  , Type(..)
  , Name(..)
    -- * Construction
  , named
  , allOfType
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
data Root = C | G | D | A | E | B | F# | Gb | Db | Ab | Eb | Bb | F
  deriving stock (Eq, Ord, Enum, Bounded)
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
    Ab -> "A♭"
    Eb -> "E♭"
    Bb -> "B♭"
    F  -> "F"

allRoots :: [Root]
allRoots = [minBound .. maxBound]

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
    F# -> ["F♯" , "G♯" , "A♯" , "B"  , "C♯" , "D♯" , "E♯"]
    Gb -> ["G♭" , "A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F" ]
    Db -> ["D♭" , "E♭" , "F"  , "G♭" , "A♭" , "B♭" , "C" ]
    Ab -> ["A♭" , "B♭" , "C"  , "D♭" , "E♭" , "F"  , "G" ]
    Eb -> ["E♭" , "F"  , "G"  , "A♭" , "B♭" , "C"  , "D" ]
    Bb -> ["B♭" , "C"  , "D"  , "E♭" , "F"  , "G"  , "A" ]
    F  -> ["F"  , "G"  , "A"  , "B♭" , "C"  , "D"  , "E" ]

-- | Natural minor scale
--
-- This introduces double-flats in some scales (G♭ and D♭).
naturalMinor :: Root -> [Note]
naturalMinor = \case
    C  -> ["C"  , "D"  , "E♭" , "F"  , "G"  , "A♭" , "B♭"]
    G  -> ["G"  , "A"  , "B♭" , "C"  , "D"  , "E♭" , "F" ]
    D  -> ["D"  , "E"  , "F"  , "G"  , "A"  , "B♭" , "C" ]
    A  -> ["A"  , "B"  , "C"  , "D"  , "E"  , "F"  , "G" ]
    E  -> ["E"  , "F♯" , "G"  , "A"  , "B"  , "C"  , "D" ]
    B  -> ["B"  , "C♯" , "D"  , "E"  , "F♯" , "G"  , "A" ]
    F# -> ["F♯" , "G♯" , "A"  , "B"  , "C♯" , "D"  , "E" ]
    Gb -> ["G♭" , "A♭" , "B𝄫" , "C♭" , "D♭" , "E𝄫" , "F♭"]
    Db -> ["D♭" , "E♭" , "F♭" , "G♭" , "A♭" , "B𝄫" , "C♭"]
    Ab -> ["A♭" , "B♭" , "C♭" , "D♭" , "E♭" , "F♭" , "G♭"]
    Eb -> ["E♭" , "F"  , "G♭" , "A♭" , "B♭" , "C♭" , "D♭"]
    Bb -> ["B♭" , "C"  , "D♭" , "E♭" , "F"  , "G♭" , "A♭"]
    F  -> ["F"  , "G"  , "A♭" , "B♭" , "C"  , "D♭" , "E♭"]

allOfType :: Type -> [Scale]
allOfType typ = [named (Name root typ) | root <- allRoots]
