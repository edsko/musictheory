-- | Unnamed chords (collection of notes)
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
-- > import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
module MusicTheory.Chord.Unnamed (
    -- * Basic definitions
    Chord(..)
    -- * Query
  , getNotes
  , size
    -- * Construction
  , fromScaleDegrees
  , wrtScale
    -- * Playable range
  , inRange
  , moveToRange
  ) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Basic definition
-------------------------------------------------------------------------------}

data Chord (r :: ReferenceKind) = Chord (NonEmpty (Reference r))

deriving instance IsReferenceKind r => Show (Chord r)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance TransposeOctave (Chord Abs) where
  transposeOctave d (Chord ns) = Chord $ transposeOctave d <$> ns

-- | Distance between two chords
--
-- For now this is a pretty simplistic definition: we merely compute the
-- distance in semitones between corresponding pairs of notes.
instance Distance (Chord Abs) where
  distance (Chord as) (Chord bs) = sum $ NE.zipWith distance as bs

instance Invert (Chord Abs) where
  invert = \(Inversion i) (Chord ns) -> Chord $
      case (i, ns) of
        (0, _)        -> ns
        (_, n :| ns') -> go (moveUp n :| []) (i - 1) ns'
    where
      moveUp :: Note.InOctave -> Note.InOctave
      moveUp = transposeOctave (OctaveShift 1)

      go ::
           NonEmpty Note.InOctave -- Notes already moved to the next octave
        -> Word                   -- Remaining number of inversions
        -> [Note.InOctave]        -- Remaining notes
        -> NonEmpty Note.InOctave
      go acc 0 ns     = NE.prependList ns (NE.reverse acc)
      go acc _ []     = NE.reverse acc
      go acc i (n:ns) = go (NE.cons (moveUp n) acc) (i - 1) ns

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getNotes :: Chord r -> NonEmpty (Reference r)
getNotes (Chord notes) = notes

-- | Number of notes in the chord
size :: Chord r -> Word
size (Chord ns) = fromIntegral $ length ns


{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromScaleDegrees :: NonEmpty Scale.Degree -> Chord Rel
fromScaleDegrees = Chord

wrtScale :: Scale.Scale -> Octave -> Chord Rel -> Chord Abs
wrtScale = \scale initOctave (Chord scaleDegrees) -> Chord $
      let go :: Octave -> NonEmpty Note -> NonEmpty Note.InOctave
          go o (n :| [])    = Note.InOctave o n :| []
          go o (n :| n':ns) = NE.cons (Note.InOctave o n) $
                                if nextOctave n n' then go (succ o) (n' :| ns)
                                                   else go       o  (n' :| ns)

      in go initOctave $ fmap (Scale.at scale) scaleDegrees
    where
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
      -- Here we want to jump to the next octave on the C♭, even though C♭ is
      -- not a \"lower\" note than A♭. Indeed, if this was
      --
      -- > D♭ F A♭ B
      --
      -- we would not want to jump to the next octave, even though C♭ and B are
      -- the same note: the spelling matters, and we should ignore accidentals.
      nextOctave :: Note -> Note -> Bool
      nextOctave n n' = Note.noteName n > Note.noteName n'

{-------------------------------------------------------------------------------
  Playable range
-------------------------------------------------------------------------------}

inRange :: (Note.InOctave, Note.InOctave) -> Chord Abs -> Bool
inRange (rangeLo, rangeHi) (Chord notes) =
    all (\n -> rangeLo <= n && n <= rangeHi) notes

-- | Move the chord into the specified range by shifting the octave
--
-- Returns 'Nothing' if this is not possible.
moveToRange ::
     (Note.InOctave, Note.InOctave)
  -> Chord Abs -> Maybe OctaveShift
moveToRange (rangeLo, rangeHi) chord@(Chord notes) = do
    let shifted = transposeOctave octaveShift chord
    guard $ inRange (rangeLo, rangeHi) shifted
    return octaveShift
  where
    lowestNote :: Note.InOctave
    lowestNote = minimum notes

    -- Distance between the lowest note in the range and the lowest actual note
    --
    -- Suppose the lowest note is /above/ 'rangeLo':
    --
    -- * If we shift the notes down by this amount, the chord would start /on/
    --   the lowest note in the range (of course that could change its pitch)
    -- * If we shift it by /more/ than this, we would exceed the lower end of
    --   the range.
    --
    -- Conversely, if the lowest note is /below/ 'rangeLo', we have to shift by
    -- /at least/ this distance.
    distanceToLo :: Word
    distanceToLo = distance rangeLo lowestNote

    octaveShift :: OctaveShift
    octaveShift = OctaveShift $
         if lowestNote > rangeLo
           then negate $ fromIntegral distanceToLo `div` 12
           else if distanceToLo `mod` 12 == 0
                   then fromIntegral distanceToLo `div` 12
                   else fromIntegral distanceToLo `div` 12 + 1

