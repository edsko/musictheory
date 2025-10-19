-- | Notes
--
-- Intended for qualified import.
--
-- > import ChordExercises.Note (Note)
-- > import ChordExercises.Note qualified as Note
module ChordExercises.Note (
    -- * Basic definitions
    Name(..)
  , Accidental(..)
  , Note(..)
    -- * \"Simple\" notes
  , SimpleAccidental(..)
  , Simple(..)
  , simpleWithAccidental
    -- * Normal forms
  , Norm(..)
  , Normalize(..)
  , fromNorm
    -- * Transposition
  , Transpose(..)
  ) where

import ChordExercises.Util

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Note normal form

  There are many enharmonic notes (E♯ == F, F𝄪 = G, etc.); how we denote these
  can only be determined in relation to a particular context (e.g. a scale).
  The normal form, intentially abstract, assigns a unique number to each note.

  (Throughout this development we assume no microtonality.)
-------------------------------------------------------------------------------}

-- | Note normal form
--
-- C = 0, C#/Db = 1, D = 2, .., B = 11.
newtype Norm = Norm Word
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Transposition
-------------------------------------------------------------------------------}

class Transpose a where
  -- | Transpose by @n@ semitones (up or down)
  transpose :: Int -> a -> a

instance Transpose Norm where
  transpose n (Norm norm) = Norm . fromIntegral $
      (fromIntegral norm + n) `mod` 12

instance Transpose a => Transpose [a] where
  transpose = map . transpose

{-------------------------------------------------------------------------------
  To normalized notes
-------------------------------------------------------------------------------}

class Normalize a where
  normalize :: a -> Norm

instance Normalize Name where
  normalize = \case
      C -> Norm 0
      D -> Norm 2
      E -> Norm 4
      F -> Norm 5
      G -> Norm 7
      A -> Norm 9
      B -> Norm 11

instance Normalize Note where
  normalize (Note name acc) =
      transpose (maybe 0 shift acc) $ normalize name
    where
      shift :: Accidental -> Int
      shift = \case
          Sharp       ->  1
          Flat        -> -1
          DoubleSharp ->  2
          DoubleFlat  -> -2
          Natural     ->  0

instance Normalize Simple where
  normalize (Simple name acc) =
      transpose (maybe 0 shift acc) $ normalize name
    where
      shift :: SimpleAccidental -> Int
      shift = \case
          SimpleSharp ->  1
          SimpleFlat  -> -1

{-------------------------------------------------------------------------------
  From normalized notes
-------------------------------------------------------------------------------}

-- | Construct note from normalized note, using specified policy
fromNorm :: SimpleAccidental -> Norm -> Simple
fromNorm SimpleSharp = fromNormSharp
fromNorm SimpleFlat  = fromNormFlat

-- | Note from normalized note, defaulting to 'Sharp' for accidentals
fromNormSharp :: Norm -> Simple
fromNormSharp (Norm norm) =
    case norm of
      0  -> "C"
      1  -> "C♯"
      2  -> "D"
      3  -> "D♯"
      4  -> "E"
      5  -> "F"
      6  -> "F♯"
      7  -> "G"
      8  -> "G♯"
      9  -> "A"
      10 -> "A♯"
      11 -> "B"
      _  -> error "invalid Norm"

-- | Note from normalized note, defaulting to 'Flat' for accidentals
fromNormFlat :: Norm -> Simple
fromNormFlat (Norm norm) =
    case norm of
      0  -> "C"
      1  -> "D♭"
      2  -> "D"
      3  -> "E♭"
      4  -> "E"
      5  -> "F"
      6  -> "G♭"
      7  -> "G"
      8  -> "A♭"
      9  -> "A"
      10 -> "B♭"
      11 -> "B"
      _  -> error "invalid Norm"
