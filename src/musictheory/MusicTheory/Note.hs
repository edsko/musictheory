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
    -- * Combinators
  , withAccidental
  , simplify
    -- * Octaves
  , InOctave(..)
  ) where

import Data.Ord
import Data.Tuple (swap)
import GHC.Stack

import MusicTheory
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Util
import MusicTheory.Util.StringTable

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
  Octaves
-------------------------------------------------------------------------------}

-- | Scientific pitch notation
data InOctave = InOctave {
      octave :: Octave
    , note   :: Note
    }
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

instance Ord InOctave where
  compare = comparing normalize

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

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Add accidental
--
-- NOTE: This is a partial function: we only support up to double sharps and
-- flats (sharpening something that is already a double sharp results in an
-- exception).
withAccidental :: HasCallStack => Note -> Maybe Accidental -> Note
withAccidental = \(Note name atal) atal' ->
    Note name (aux atal atal')
  where
    aux :: Maybe Accidental -> Maybe Accidental -> Maybe Accidental
    aux atal        Nothing      = atal
    aux Nothing     atal'        = atal'
    aux (Just atal) (Just atal') = Just $
        case (atal, atal') of
          (Sharp       , Sharp      ) -> DoubleSharp
          (Sharp       , Flat       ) -> Natural
          (Flat        , Sharp      ) -> Natural
          (Flat        , Flat       ) -> DoubleFlat
          (DoubleSharp , Flat       ) -> Sharp
          (DoubleFlat  , Sharp      ) -> Flat
          (Flat        , DoubleSharp) -> Sharp
          (Sharp       , DoubleFlat ) -> Flat
          _otherwise -> error $ "withAccidental: " ++ show (atal, atal')

simplify :: InOctave -> InOctave
simplify note@(InOctave o (Note name atal)) =
    case (name, atal) of

      --
      -- Already in simplest form
      --

      (C, Nothing)    -> note
      (C, Just Sharp) -> note ; (D, Just Flat)  -> note
      (D, Nothing)    -> note
      (D, Just Sharp) -> note ; (E, Just Flat)  -> note
      (E, Nothing)    -> note
      (F, Nothing)    -> note
      (F, Just Sharp) -> note ; (G, Just Flat)  -> note
      (G, Nothing)    -> note
      (G, Just Sharp) -> note ; (A, Just Flat)  -> note
      (A, Nothing)    -> note
      (A, Just Sharp) -> note ; (B, Just Flat)  -> note
      (B, Nothing)    -> note

      --
      -- Simplify "simple" accidentals
      --

      (C, Just Flat)  -> InOctave (o - 1) $ Note B Nothing
      (F, Just Flat)  -> InOctave  o      $ Note E Nothing

      (E, Just Sharp) -> InOctave  o      $ Note F Nothing
      (B, Just Sharp) -> InOctave (o + 1) $ Note C Nothing

      --
      -- Strip explicit naturals
      --

      (_, Just Natural) -> InOctave o $ Note name Nothing

      --
      -- Double flats and sharps
      --

      (C, Just DoubleFlat) -> InOctave (o - 1) $ Note B (Just Flat)
      (D, Just DoubleFlat) -> InOctave  o      $ Note C Nothing
      (E, Just DoubleFlat) -> InOctave  o      $ Note D Nothing
      (F, Just DoubleFlat) -> InOctave  o      $ Note E (Just Flat)
      (G, Just DoubleFlat) -> InOctave  o      $ Note F Nothing
      (A, Just DoubleFlat) -> InOctave  o      $ Note G Nothing
      (B, Just DoubleFlat) -> InOctave  o      $ Note A Nothing

      (C, Just DoubleSharp) -> InOctave  o      $ Note D Nothing
      (D, Just DoubleSharp) -> InOctave  o      $ Note E Nothing
      (E, Just DoubleSharp) -> InOctave  o      $ Note F (Just Sharp)
      (F, Just DoubleSharp) -> InOctave  o      $ Note G Nothing
      (G, Just DoubleSharp) -> InOctave  o      $ Note A Nothing
      (A, Just DoubleSharp) -> InOctave  o      $ Note B Nothing
      (B, Just DoubleSharp) -> InOctave (o + 1) $ Note C (Just Sharp)
