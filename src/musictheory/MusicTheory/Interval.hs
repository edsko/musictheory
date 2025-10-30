-- | Intervals
--
-- Intended for qualified import.
--
-- > import MusicTheory.Interval (Interval)
-- > import MusicTheory.Interval qualified as Interval
module MusicTheory.Interval (
    Interval(..)
  , toSemitones
  , fromMajorScaleDegree
    -- Standard intervals
  , unison
  , minorSecond
  , majorSecond
  , minorThird
  , majorThird
  , perfectFourth
  , perfectFifth
  , augmentedFifth
  , minorSixth
  , majorSixth
  , minorSeventh
  , majorSeventh
  , octave
  ) where

import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale
import MusicTheory.Util.StringTable

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Interval between notes in semitones
--
-- There are multiple names for the same interval; such as "♯5" and "♭6". The
-- difference is only relevant when compared to another interval. For example,
-- it is more useful to describe the voicing of a minor triad as "1", "♭3", "5"
-- rather than "♯2". (We assume no microtonality.)
data Interval = Interval Word (Maybe Note.SimpleAccidental)
  deriving stock (Eq)
  deriving (Show, IsString) via UseStringTable Interval

instance HasStringTable Interval where
  stringTable = uncurry (flip Interval) <$>
      stringTablePair
        (stringTableMaybe "" stringTable)
        (stringTableNum [1..14])

toSemitones :: Interval -> Word
toSemitones (Interval n atal) =
    maybe id adjust atal $ n * 2
  where
    adjust :: Note.SimpleAccidental -> Word -> Word
    adjust Note.SimpleSharp semitones = semitones + 1
    adjust Note.SimpleFlat  semitones = semitones - 1

-- | Interval for scale degree
--
-- Although the definition of 'Interval' is identical to the definition of
-- 'Scale.Degree', the two types are not interchangeable. An interval of a
-- perfect third, written "3", is always 4 semitones, but scale interval "3"
-- might be 4 semitones (wrt to a major scale), 3 semitones (wrt to a minor
-- scale), or indeed an arbitrary other interval for more exotic scales.
--
-- For the major scale, however, they line up one to one.
fromMajorScaleDegree :: Scale.Degree -> Interval
fromMajorScaleDegree (Scale.Degree d atal) = Interval d atal

{-------------------------------------------------------------------------------
  Standard intervals

  This is just a selection of common interval names; there are many more.

  <https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals>
-------------------------------------------------------------------------------}

unison :: Interval
unison = "1"

minorSecond, majorSecond :: Interval
minorSecond = "♭2"
majorSecond = "2"

minorThird, majorThird :: Interval
minorThird = "♭3"
majorThird = "3"

perfectFourth :: Interval
perfectFourth = "4"

perfectFifth, augmentedFifth :: Interval
perfectFifth   = "5"
augmentedFifth = "♯5"

minorSixth, majorSixth  :: Interval
minorSixth = "♭6"
majorSixth = "6"

minorSeventh, majorSeventh :: Interval
minorSeventh = "♭7"
majorSeventh = "7"

octave :: Interval
octave = "8"
