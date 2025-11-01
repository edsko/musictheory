-- | Utilities for generating chord exercises
--
-- Intended for qualified import.
--
-- > import Exercises.Chords qualified as Chords
module Exercises.Chords (
    Setup(..)
  , Exercise(..)
  , exercise
  ) where

import Data.Default
import Data.Functor

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave (Octave)
import MusicTheory.Reference
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.ChordInversion qualified as ChordInversion

{-------------------------------------------------------------------------------
  Construct chords exercise

  We show these chords as the /I/ chord in all /major/ scales.
-------------------------------------------------------------------------------}

data Setup = Setup{
      title         :: String
    , intro         :: Maybe Ly.Markup
    , clef          :: Ly.Clef
    , numInversions :: Int -- ^ Number of inversions for each chord
    }

data Exercise = Exercise{
      chordType :: Chord.Type
    , voicing   :: Voicing

      -- | Starting octave for building the chord
    , startingOctave :: Octave

      -- | Adjust octave after inversion
      --
      -- This can be used to place the final chord somewhere more suitable on
      -- the stave. If this returns 'Nothing', we conclude that this particular
      -- inversion is \"outside the playable range\" and omit it.
    , adjustOctave :: Named.Chord Abs -> Maybe OctaveShift

      -- | Inversions
      --
      -- This should not exceed 'numInversions'
    , inversionsFor :: Scale.Root -> [ChordInversion]
    }

exercise :: Scale.Type -> Setup -> Exercise -> [Ly.SectionElem]
exercise scaleType setup ex = [
      Ly.SectionScore Ly.Score{
          title = Just setup.title
        , intro = setup.intro
        , staff = Ly.Staff{
              props = staffProps
            , elems = mconcat [
                  exerciseIn scaleType ex firstHalf
                , [Ly.StaffLinebreak]
                , exerciseIn scaleType ex secondHalf
                ]
            }
        }
    , Ly.SectionScore Ly.Score{
          title = Nothing
        , intro = Just $ Ly.Markup.italic $ Ly.Markup.fontsize 8 $ "Enharmonic"
        , staff = Ly.Staff{
              props = staffProps
            , elems = exerciseIn scaleType ex enharmonic
            }
        }
    ]
  where
    staffProps :: Ly.StaffProps
    staffProps = def{
          Ly.clef               = setup.clef
        , Ly.timeSignature      = Ly.TimeSignature setup.numInversions 1
        , Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

    firstHalf, secondHalf, enharmonic :: [Scale.Root]
    (firstHalf, secondHalf, enharmonic) =
      case scaleType of
        Scale.Major -> (
            take 6 $ Scale.defaultRoots Scale.Major
          , drop 6 $ Scale.defaultRoots Scale.Major
          , Scale.enharmonicRoots Scale.Major
          )
        Scale.Minor -> (
            take 6 $ Scale.defaultRoots Scale.Minor
          , drop 6 $ Scale.defaultRoots Scale.Minor
          , Scale.enharmonicRoots Scale.Minor
          )

exerciseIn :: Scale.Type -> Exercise -> [Scale.Root] -> [Ly.StaffElem]
exerciseIn scaleType ex scales =
    concatMap goScale scales
  where
    -- Show all inversions for the specified scale
    --
    -- We show the chord name only once
    goScale :: Scale.Root -> [Ly.StaffElem]
    goScale scaleRoot =
        zipWith
          (goInversion chord')
          (ex.inversionsFor scaleRoot)
          (True : repeat False)
      where
        scale :: Scale
        scale = Scale.named $ Scale.Name scaleRoot scaleType

        chord :: Named.Chord Rel
        chord = Chord.Named.chordI ex.chordType

        chord' :: Named.Chord Abs
        chord' = Voicing.wrtScale scale ex.voicing ex.startingOctave chord

    goInversion ::
         Named.Chord Abs
      -> ChordInversion
      -> Bool  -- Show chord name?
      -> Ly.StaffElem
    goInversion chord chordInversion showChordName =
        case mShifted of
          Nothing -> Ly.StaffRest Ly.Rest{
              duration
            , name       = mName
            , annotation = ann
              }
          Just shifted -> Ly.StaffChord Ly.Chord{
              notes      = Chord.Named.getNotes shifted
            , duration
            , name       = mName
            , annotation = ann
            }
      where
        ChordInversion{annotation = ann} = chordInversion

        inverted :: Named.Chord Abs
        inverted = ChordInversion.apply chordInversion chord

        mShifted :: Maybe (Named.Chord Abs)
        mShifted =
            ex.adjustOctave inverted <&> \shift ->
              transposeOctave shift inverted

        mName :: Maybe (Chord.Name Abs)
        mName | showChordName = Just $ Chord.Named.getName chord
              | otherwise     = Nothing

    duration :: Ly.Duration
    duration = Ly.OneOver 1

