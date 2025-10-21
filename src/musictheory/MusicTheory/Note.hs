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
    -- * \"Simple\" notes
  , SimpleAccidental(..)
  , fromSimpleAccidental
  , Simple(..)
  , simpleWithAccidental
  , fromSimple
    -- * Octaves
  , InOctave(..)
  , Octave(..)
  , middleOctave
  , aboveMiddleOctave
  ) where

import MusicTheory
import MusicTheory.Util.StringTable
import MusicTheory.Util

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

{-------------------------------------------------------------------------------
  "Simple" notes
-------------------------------------------------------------------------------}

data SimpleAccidental = SimpleSharp | SimpleFlat
  deriving stock (Eq, Enum, Bounded)
  deriving (Show, IsString) via UseStringTable SimpleAccidental

fromSimpleAccidental :: SimpleAccidental -> Accidental
fromSimpleAccidental SimpleSharp = Sharp
fromSimpleAccidental SimpleFlat  = Flat

instance HasStringTable SimpleAccidental where
  stringTable = stringTableEnum $ \case
      SimpleSharp -> "♯"
      SimpleFlat  -> "♭"

-- | \"Simple\" notes
--
-- Simple notes have simple accidentals: at most a single sharp or a single
-- flat. This is a useful concept, because when we sharpen or flatten a simple
-- note, we at most need a double sharp or double flat.
data Simple = Simple Name (Maybe SimpleAccidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Simple

instance HasStringTable Simple where
  stringTable = uncurry Simple <$>
      stringTablePair stringTable (stringTableMaybe "" stringTable)

simpleWithAccidental :: Simple -> Maybe SimpleAccidental -> Note
simpleWithAccidental = \(Simple note atal) atal' ->
    Note note (aux atal atal')
  where
    aux :: Maybe SimpleAccidental -> Maybe SimpleAccidental -> Maybe Accidental
    aux atal        Nothing      = fromSimpleAccidental <$> atal
    aux Nothing     atal'        = fromSimpleAccidental <$> atal'
    aux (Just atal) (Just atal') = Just $
        case (atal, atal') of
          (SimpleSharp, SimpleSharp) -> DoubleSharp
          (SimpleSharp, SimpleFlat)  -> Natural
          (SimpleFlat,  SimpleSharp) -> Natural
          (SimpleFlat,  SimpleFlat)  -> DoubleFlat

fromSimple :: Simple -> Note
fromSimple (Simple name atal) = Note name (fromSimpleAccidental <$> atal)

{-------------------------------------------------------------------------------
  Octaves
-------------------------------------------------------------------------------}

-- | Scientific pitch notation
data InOctave = InOctave Note Octave
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable InOctave

instance HasStringTable InOctave where
  stringTable = uncurry InOctave <$> stringTablePair stringTable stringTable

-- | Octave
newtype Octave = Octave Word
  deriving stock (Eq)
  deriving newtype (Enum)
  deriving (Show, IsString) via UseStringTable Octave

instance Bounded Octave where
  minBound = Octave 0
  maxBound = Octave 9

instance HasStringTable Octave where
  stringTable = stringTableEnum $ \(Octave o) ->
      case o of
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> error "Invalid Octave"

instance TransposeOctave InOctave where
  transposeOctave (OctaveShift d) (InOctave n (Octave o)) =
      InOctave n . Octave $ shiftIntegral d o

-- | Octave containing middle C
middleOctave :: Octave
middleOctave = Octave 4

-- | How many octaves is the specified octave about the middle octave?
aboveMiddleOctave :: Octave -> Int
aboveMiddleOctave (Octave o) = fromIntegral o - 4

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

instance Normalize Simple where
  normalize (Simple name atal) = Norm $
      shiftIntegral (maybe 0 shift atal) $ semitones (normalize name)
    where
      shift :: SimpleAccidental -> Int
      shift = \case
          SimpleSharp ->  1
          SimpleFlat  -> -1

instance Normalize Octave where
  normalize (Octave o) = Norm $ o * 12

instance Normalize InOctave where
  normalize (InOctave note octave) = Norm $
        semitones (normalize octave)
      + semitones (normalize note)

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
deriving via NormalForm Simple   instance Distance Simple
deriving via NormalForm InOctave instance Distance InOctave
