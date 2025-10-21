-- | Chord progressions
--
-- Intended for qualified import.
--
-- > import MusicTheory.Progression (Progression(Progression))
-- > import MusicTheory.Progression qualified as Progression
module MusicTheory.Progression (
    Progression(..)
    -- * Construction
  , wrtMajorScale
    -- * Combinators
  , voiceLeading
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord.Name qualified as Chord.Name
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Type qualified as Chord (Type)
import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
import MusicTheory.Note qualified as Note
import MusicTheory.Progression.Name qualified as Progression (Name)
import MusicTheory.Progression.Name qualified as Progression.Name
import MusicTheory.Scale qualified as Scale
import MusicTheory.Util

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

newtype Progression = Progression (NonEmpty Named.Chord)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

wrtMajorScale :: Note.Octave -> Scale.Name -> Progression.Name -> Progression
wrtMajorScale octave scale =
    Progression . fmap aux . Progression.Name.wrtMajorScale
  where
    aux :: (Chord.Type, NonEmpty Scale.Degree) -> Named.Chord
    aux (chordType, scaleDegrees) = Named.Chord{
          name  = Chord.Name.chordNth (Scale.majorScale scale) root chordType
        , notes = Chord.Unnamed.fromScaleDegrees octave scale scaleDegrees
        }
      where
        root :: Scale.Degree
        root = NE.head scaleDegrees

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Choose inversions to minimize distance between successive chords
--
-- Fails if there is no unique solution.
voiceLeading :: [Inversion] -> Progression -> Progression
voiceLeading permissibleInversions = \(Progression chords) -> Progression $
    case chords of
      c :| cs -> c :| go c cs
  where
    go :: Named.Chord -> [Named.Chord] -> [Named.Chord]
    go _    []        = []
    go prev (next:cs) =
        let next' = minimize (distance prev) allOptions
         in next' : go next' cs
      where
        possibleInversions :: [Named.Chord]
        possibleInversions = map (flip invert next) permissibleInversions

        -- We consider all inversions in their \"natural\" octave, as well as
        -- one octave longer (because inversion tends to move everything up).
        allOptions :: [Named.Chord]
        allOptions = concatMap addOctaveDown possibleInversions
          where
            addOctaveDown :: Named.Chord -> [Named.Chord]
            addOctaveDown c = [c, transposeOctave (OctaveShift (-1)) c]