-- | Notes
--
-- Intended for qualified import.
--
-- > import MusicTheory.Note (Note(Note))
-- > import MusicTheory.Note qualified as Note
module MusicTheory.Note (
    -- * Basic definitions
    Name(..)
  , Accidental(..)
  , Note(..)
  , noteName
  , withAccidental
    -- * Octaves
  , InOctave(..)
  ) where

import MusicTheory
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Util
import MusicTheory.Util.StringTable
import Data.Tuple (swap)

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

-- | Note names
--
-- The 'Ord' instance corresponds to the relative position in the major scale.
data Name = C | D | E | F | G | A | B
  deriving stock (Show, Eq, Ord, Enum, Bounded)
  deriving HasStringTable via UseShow Name
  deriving IsString via UseStringTable Name

data Accidental = Sharp | Flat | DoubleSharp | DoubleFlat | Natural
  deriving stock (Eq, Enum, Bounded)
  deriving (Show, IsString) via UseStringTable Accidental

instance HasStringTable Accidental where
  stringTable = stringTableEnum $ \case
      Sharp       -> "♯"
      Flat        -> "♭"
      DoubleSharp -> "𝄪"
      DoubleFlat  -> "𝄫"
      Natural     -> "♮"

data Note = Note Name (Maybe Accidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Note

instance HasStringTable Note where
  stringTable = uncurry Note <$>
      stringTablePair stringTable (stringTableMaybe "" stringTable)

noteName :: Note -> Name
noteName (Note name _atal) = name

-- | Add accidental
--
-- NOTE: This is a partial function: we only support up to double sharps and
-- flats (sharpening something that is already a double sharp results in an
-- exception).
withAccidental :: Note -> Maybe Accidental -> Note
withAccidental = \(Note name atal) atal' ->
    Note name (aux atal atal')
  where
    aux :: Maybe Accidental -> Maybe Accidental -> Maybe Accidental
    aux atal        Nothing      = atal
    aux Nothing     atal'        = atal'
    aux (Just atal) (Just atal') = Just $
        case (atal, atal') of
          (Sharp, Sharp) -> DoubleSharp
          (Sharp, Flat)  -> Natural
          (Flat,  Sharp) -> Natural
          (Flat,  Flat)  -> DoubleFlat
          _otherwise     -> error $ "Unsupported " ++ show (atal, atal')

{-------------------------------------------------------------------------------
  Octaves
-------------------------------------------------------------------------------}

-- | Scientific pitch notation
data InOctave = InOctave Octave Note
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable InOctave

instance HasStringTable InOctave where
  stringTable =
      -- Show the octave /after/ the note
      uncurry InOctave . swap <$>
        stringTablePair stringTable stringTable

instance TransposeOctave InOctave where
  transposeOctave (OctaveShift d) (InOctave (Octave o) n) =
      InOctave (Octave $ shiftIntegral d o) n

{-------------------------------------------------------------------------------
  Distance
-------------------------------------------------------------------------------}

-- | Internal: normal form
--
-- The sole purpose of normalization is to be able to easily compute the
-- distance in semitones between two notes. Normalized forms of different types
-- should be considered incomparable.
--
-- (Throughout this development we assume no microtonality.)
class Normalize a where
  normalize :: a -> Norm a

-- | Normal form
--
-- See 'Normalize'
newtype Norm a = Norm { semitones :: Word }
  deriving stock (Show, Eq, Ord)

instance Normalize Name where
  normalize = Norm . \case
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11

-- | Normalize 'Note'
--
-- > C = 0, C#/Db = 1, D = 2, .., B = 11.
--
-- There is no meaningful operation in the reverse direction! Starting from
-- normalized note @5@, should we call this @F@, or @E#@?
instance Normalize Note where
  normalize (Note name atal) = Norm $
      shiftIntegral (maybe 0 shift atal) $ semitones (normalize name)
    where
      shift :: Accidental -> Int
      shift = \case
          Sharp       ->  1
          Flat        -> -1
          DoubleSharp ->  2
          DoubleFlat  -> -2
          Natural     ->  0

instance Normalize InOctave where
  normalize (InOctave (Octave o) note) = Norm $
        (o * 12) + semitones (normalize note)

-- | Deriving-via helper to derive 'Distance'
newtype NormalForm a = NormalForm a

instance Normalize a => Distance (NormalForm a) where
  distance (NormalForm a) (NormalForm b) =
      fromIntegral $ abs (semA - semB)
    where
      semA, semB :: Int
      semA = fromIntegral . semitones $ normalize a
      semB = fromIntegral . semitones $ normalize b

deriving via NormalForm Name     instance Distance Name
deriving via NormalForm Note     instance Distance Note
deriving via NormalForm InOctave instance Distance InOctave
