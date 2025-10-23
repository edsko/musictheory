{-# LANGUAGE OverloadedLists #-}

-- | Chord progressions
--
-- Intended for qualified import.
--
-- > import MusicTheory.Progression (Progression(..))
-- > import MusicTheory.Progression qualified as Progression
module MusicTheory.Progression (
    Progression(..)
    -- * Combinators
  , voiceLeading
    -- * Standard chord progressions
  , Name(..)
  , named
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Reference
import MusicTheory.Util

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

newtype Progression r = Progression (NonEmpty (Named.Chord r))
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance MakeAbsolute Progression where
  wrtScale octave scale (Progression chords) = Progression $
      wrtScale octave scale <$> chords

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Choose inversions to minimize distance between successive chords
--
-- Fails if there is no unique solution.
voiceLeading ::
     (Chord.Type -> [Inversion]) -- ^ Permissible inversions
  -> Progression Absolute -> Progression Absolute
voiceLeading permissibleInversions = \(Progression chords) -> Progression $
    case chords of
      c :| cs -> c :| go c cs
  where
    go ::
         Named.Chord Absolute  -- Previous chord (for voice leading)
      -> [Named.Chord Absolute] -> [Named.Chord Absolute]
    go _    []        = []
    go prev (next:cs) =
        let next' = minimize (distance prev) allOptions
         in next' : go next' cs
      where
        possibleInversions :: [Named.Chord Absolute]
        possibleInversions = [
              invert i next
            | i <- permissibleInversions (Chord.Named.getType next)
            ]

        -- We consider all inversions in their \"natural\" octave, as well as
        -- one octave longer (because inversion tends to move everything up).
        allOptions :: [Named.Chord Absolute]
        allOptions = concatMap addOctaveDown possibleInversions
          where
            addOctaveDown :: Named.Chord Absolute -> [Named.Chord Absolute]
            addOctaveDown c = [c, transposeOctave (OctaveShift (-1)) c]

{-------------------------------------------------------------------------------
  Standard progressions
-------------------------------------------------------------------------------}

-- | Standard progressions
data Name =
    -- | Using standard Jazz voicing
    StdJazz_Major251
  | StdJazz_Minor251

named :: Name -> Progression Relative
named = Progression . fmap Chord.Named.relative . \case
    StdJazz_Major251 -> [
        Chord.Name "2" Chord.StdJazz_Minor
      , Chord.Name "5" Chord.StdJazz_Dominant
      , Chord.Name "1" Chord.StdJazz_Major
      ]
    StdJazz_Minor251 -> [
        Chord.Name "2" Chord.StdJazz_HalfDiminished
      , Chord.Name "5" Chord.StdJazz_AlteredFlat9
      , Chord.Name "1" Chord.StdJazz_Minor
      ]