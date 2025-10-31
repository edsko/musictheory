-- | Utilities for generating chord exercises
module Exercises.Chords (
    -- * Construct exercise
    ChordExercise(..)
  , chordExercise
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

data ChordExercise = ChordExercise{
      title     :: String
    , intro     :: Maybe Ly.Markup
    , clef      :: Ly.Clef
    , chordType :: Chord.Type
    , voicing   :: Voicing

      -- | Starting octave for building the chord
    , startingOctave :: Octave

      -- | How many inversions do we show for each chord?
    , numInversions  :: Int

      -- | Inversions
      --
      -- This should not exceed 'numInversions'
    , inversionsFor :: Scale.Root -> [ChordInversion]

      -- | Adjust octave after inversion
      --
      -- This can be used to place the final chord somewhere more suitable on
      -- the stave. If this returns 'Nothing', we conclude that this particular
      -- inversion is \"outside the playable range\" and omit it.
    , adjustOctave :: Named.Chord Abs -> Maybe OctaveShift
    }

chordExercise :: Scale.Type -> ChordExercise -> [Ly.SectionElem]
chordExercise scaleType exercise = [
      Ly.SectionScore Ly.Score{
          title = Just exercise.title
        , intro = exercise.intro
        , staff = Ly.Staff{
              props = staffProps
            , elems = mconcat [
                  chordsOfTypeIn scaleType exercise firstHalf
                , [Ly.StaffLinebreak]
                , chordsOfTypeIn scaleType exercise secondHalf
                ]
            }
        }
    , Ly.SectionScore Ly.Score{
          title = Nothing
        , intro = Just $ Ly.Markup.italic $ Ly.Markup.fontsize 8 $ "Enharmonic"
        , staff = Ly.Staff{
              props = staffProps
            , elems = chordsOfTypeIn scaleType exercise enharmonic
            }
        }
    ]
  where
    staffProps :: Ly.StaffProps
    staffProps = def{
          Ly.clef               = exercise.clef
        , Ly.timeSignature      = Ly.TimeSignature exercise.numInversions 1
        , Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

    (firstHalf, secondHalf, enharmonic) =
      case scaleType of
        Scale.Major -> (
            take 6 Scale.allMajorRoots
          , drop 6 Scale.allMajorRoots
          , Scale.enharmonicMajorRoots
          )
        Scale.Minor -> (
            take 6 Scale.allMinorRoots
          , drop 6 Scale.allMinorRoots
          , Scale.enharmonicMinorRoots
          )

chordsOfTypeIn :: Scale.Type -> ChordExercise -> [Scale.Root] -> [Ly.StaffElem]
chordsOfTypeIn scaleType exercise scales =
    concatMap goScale scales
  where
    -- Show all inversions for the specified scale
    --
    -- We show the chord name only once
    goScale :: Scale.Root -> [Ly.StaffElem]
    goScale scaleRoot =
        zipWith
          ( goInversion $
              Voicing.wrtScale
                scale
                exercise.voicing
                exercise.startingOctave
                chord
          )
          (exercise.inversionsFor scaleRoot)
          (True : repeat False)
      where
        scale :: Scale
        scale = Scale.named $ Scale.Name scaleRoot scaleType

        chord :: Named.Chord Rel
        chord = Chord.Named.chordI exercise.chordType

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
            exercise.adjustOctave inverted <&> \shift ->
              transposeOctave shift inverted

        mName :: Maybe (Chord.Name Abs)
        mName | showChordName = Just $ Chord.Named.getName chord
              | otherwise     = Nothing

    duration :: Ly.Duration
    duration = Ly.OneOver 1

