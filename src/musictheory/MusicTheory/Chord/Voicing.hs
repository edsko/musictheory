{-# LANGUAGE OverloadedLists #-}

-- | Chord voicings
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Voicing (Voicing)
-- > import MusicTheory.Chord.Voicing qualified as Voicing
module MusicTheory.Chord.Voicing (
    Voicing(..)
    -- * Voice chords
  , wrtScale
    -- * Expansion to scale degrees
  , scaleDegrees
  , intervals
  ) where

import Data.List.NonEmpty (NonEmpty)
import GHC.Stack

import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Named
import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Interval (Interval)
import MusicTheory.Interval qualified as Interval
import MusicTheory.Note.Octave (Octave)
import MusicTheory.Reference
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Voicing =
    Default
  | FourWayClose
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Voicing of a particular chord
-------------------------------------------------------------------------------}

wrtScale :: Scale -> Voicing -> Octave -> Named.Chord Rel -> Named.Chord Abs
wrtScale scale voicing octave (Named.Rel chordName) =
    Named.Abs
      (Chord.nameWrtScale     scale octave $ chordName)
      (Chord.Unnamed.wrtScale scale octave $ notes)
    where
      notes :: Unnamed.Chord Rel
      notes = Chord.Unnamed.fromScaleDegrees $
                scaleDegrees scale.name.typ voicing chordName

{-------------------------------------------------------------------------------
  Expansion to scale degrees
-------------------------------------------------------------------------------}

scaleDegrees :: Scale.Type -> Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
scaleDegrees Scale.Major = scaleDegreesMajor
scaleDegrees Scale.Minor = scaleDegreesMinor

intervals :: Voicing -> Chord.Type -> NonEmpty Interval
intervals voicing typ =
    Interval.fromMajorScaleDegree <$> majorDegree1 voicing typ

{-------------------------------------------------------------------------------
  Scale degrees wrt major scale
-------------------------------------------------------------------------------}

-- | Scale degrees for chord wrt to the a /major/ scale
scaleDegreesMajor :: Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
scaleDegreesMajor voicing chordName
  | chordName.root == "1" = majorDegree1     voicing chordName.typ
  | otherwise             = majorOtherDegree voicing chordName

-- | Scale degrees wrt to the major scale, for chord with root at degree 1
--
-- > majorDegree1 Default Chord.MajorTriad == ["1" ,  "3", "5"]
-- > majorDegree1 Default Chord.MinorTriad == ["1" , "♭3", "5"]
majorDegree1 :: Voicing -> Chord.Type -> NonEmpty Scale.Degree
majorDegree1 voicing typ =
    case (voicing, typ) of
      (Default, Chord.MajorTriad)     -> [ "1" ,  "3" , "5"]
      (Default, Chord.MinorTriad)     -> [ "1" , "♭3" , "5"]
      (Default, Chord.Major7)         -> [ "1" ,  "3" , "5",  "7" ]
      (Default, Chord.Minor7)         -> [ "1" , "♭3" , "5", "♭7" ]
      (Default, Chord.Dominant7)      -> [ "1" ,  "3" , "5", "♭7" ]

      (FourWayClose, Chord.Major7)         -> [ "3" ,  "5" ,  "7" ,  "9" ]
      (FourWayClose, Chord.Minor7)         -> [ "3" , "♭5" , "♭7" ,  "9" ]
      (FourWayClose, Chord.Dominant7)      -> [ "3" , "13" , "♭7" ,  "9" ]
      (FourWayClose, Chord.HalfDiminished) -> [ "1" , "♭3" , "♭5" , "♭7" ]
      (FourWayClose, Chord.Altered)        -> [ "3" , "♯5" , "♭7" , "♯9" ]
      (FourWayClose, Chord.AlteredFlat9)   -> [ "3" ,  "5" , "♭7" , "♭9" ]
      (FourWayClose, Chord.Sus)            -> [ "2" ,  "4" , "13" , "♭7" ]

      _otherwise -> notYetImplemented (voicing, typ)

majorOtherDegree :: Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
majorOtherDegree voicing Chord.Name{root, typ} =
    case (voicing, root, typ) of
      -- Major 2-5-1 using four note close voicing
      --
      -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
      -- >  C    D    E    F    G    A    B   (C)   D  ( E)   F  ( G)   A  ( B)
      -- > ------------------------------------------------
      -- >       ._________*_________*_________*_________*                    (iim7)
      -- >                      ._________*_________*_________*_________*     (V7)
      -- >  ._________*_________*_________*_________*                         (Imaj7)
      (FourWayClose, "2", Chord.Minor7)    -> [ "4" , "6" ,  "8" , "10" ]
      (FourWayClose, "5", Chord.Dominant7) -> [ "7" , "9" , "11" , "13" ]

      _otherwise -> notYetImplemented (voicing, root, typ)

{-------------------------------------------------------------------------------
  Scale degrees wrt minor scale

  We usually use a major scale as context, so we implement fewer cases here.
-------------------------------------------------------------------------------}

scaleDegreesMinor :: Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
scaleDegreesMinor voicing Chord.Name{root, typ} =
    case (voicing, root, typ) of
      -- Minor 2-5-1 using four note close voicing
      --
      -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
      -- >  C    D    E♭   F    G    A♭   B♭  (C)   D  (E♭)   F  ( G)  A♭  (B♭)
      -- > ------------------------------------------------
      -- >       *_________*_________*_________*                              (iim7.-5)
      -- >                      ._________*_________*_________*_________*     (V7.-9)
      -- >  ._________*_________*_________*_________*                         (Imaj7)
      --
      -- NOTE: The "Flat9" "AlteredFlat9" does not refer to scale degrees, but
      -- rather to the interval between its nineth (second).
      --
      -- NOTE: In the context of the (natural) minor scale, we need to sharpen
      -- scale degree 7 to get the dominant chord (rather than a minor chord).
      (FourWayClose, "2", Chord.HalfDiminished) -> [  "2" , "4" ,  "6" ,  "8" ]
      (FourWayClose, "5", Chord.AlteredFlat9)   -> [ "♯7" , "9" , "11" , "13" ]
      (FourWayClose, "1", Chord.Minor7)         -> [  "3" , "5" ,  "7" ,  "9" ]

      _otherwise -> notYetImplemented (voicing, root, typ)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Unimplemented case
--
-- We don't implement all cases here, but add new cases as they are needed.
notYetImplemented :: (HasCallStack, Show a) => a -> NonEmpty Scale.Degree
notYetImplemented x = error $ concat @[] [
      "expansion to scale degrees for "
    , show x
    , " not yet implemented"
    ]