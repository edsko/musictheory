-- | Unnamed chords (collection of notes)
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
-- > import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
module MusicTheory.Chord.Unnamed (
    -- * Basic definitions
    Chord(..)
  , size
    -- * Construction
  , fromNotes
  , fromScaleDegrees
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Basic definition
-------------------------------------------------------------------------------}

data Chord = Chord (NonEmpty Note.InOctave)
  deriving stock (Show)

-- | Number of notes in the chord
size :: Chord -> Word
size (Chord ns) = fromIntegral $ length ns

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance TransposeOctave Chord where
  transposeOctave d (Chord ns) = Chord $ transposeOctave d <$> ns

-- | Distance between two chords
--
-- For now this is a pretty simplistic definition: we merely compute the
-- distance in semitones between corresponding pairs of notes.
instance Distance Chord where
  distance (Chord as) (Chord bs) = sum $ NE.zipWith distance as bs

instance Invert Chord where
  invert = \(Inversion i) (Chord ns) -> Chord $
      case (i, ns) of
        (0, _)        -> ns
        (_, n :| ns') -> go (moveUp n :| []) (i - 1) ns'
    where
      moveUp :: Note.InOctave -> Note.InOctave
      moveUp = transposeOctave (OctaveShift 1)

      go ::
           NonEmpty Note.InOctave -- Accumulator
        -> Word                   -- Remaining number of inversions
        -> [Note.InOctave]        -- Remaining notes
        -> NonEmpty Note.InOctave
      go acc 0 ns     = NE.prependList ns (NE.reverse acc)
      go acc _ []     = NE.reverse acc
      go acc i (n:ns) = go (NE.cons (moveUp n) acc) (i - 1) ns

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromScaleDegrees :: Note.Octave -> Scale.Name -> NonEmpty Scale.Degree -> Chord
fromScaleDegrees octave scale =
    fromNotes octave . fmap fromDegree
  where
    fromDegree :: Scale.Degree -> Note
    fromDegree = Scale.fromDegree (Scale.majorScale scale)

-- | Helper function for constructing a chord, given octave for first note
fromNotes :: Note.Octave -> NonEmpty Note -> Chord
fromNotes = \o ns -> Chord $ go o ns
  where
    go :: Note.Octave -> NonEmpty Note -> NonEmpty Note.InOctave
    go o (n :| [])    = Note.InOctave n o :| []
    go o (n :| n':ns) = NE.cons (Note.InOctave n o) $
                          if nextOctave n n' then go (succ o) (n' :| ns)
                                             else go       o  (n' :| ns)

    -- Should we jump to the next octave?
    --
    -- This is a bit subtle. Consider
    --
    -- > G B D
    --
    -- In this case, we want to jump to the next octave when we see the D.
    -- Intuitively, this is because D is a \"lower\" note than B, and so it
    -- should be placed in the octave up. However, we should not just call
    -- 'normalize' on the notes to see which note is \"lower\"; consider:
    --
    -- > D♭ F A♭ C♭
    --
    -- Here we want to jump to the next octave on the C♭, even though C♭ is not
    -- a \"lower\" note than A♭. Indeed, if this was
    --
    -- > D♭ F A♭ B
    --
    -- we would /not/ want to jump to the next octave, even though C♭ and B are
    -- the same note: the spelling matters, and we should ignore accidentals.
    nextOctave :: Note -> Note -> Bool
    nextOctave n n' = Note.noteName n > Note.noteName n'
