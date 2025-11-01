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

import Data.List.NonEmpty (NonEmpty(..))
import GHC.Stack

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Named
import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Interval (Interval)
import MusicTheory.Interval qualified as Interval
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Note.Octave qualified as Octave
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

      (FourWayClose, Chord.Major7)         -> [         "3" ,   "5" ,  "7" ,  "9" ]
      (FourWayClose, Chord.Minor7)         -> [        "♭3" ,   "5" , "♭7" ,  "9" ]
      (FourWayClose, Chord.Dominant7)      -> [         "3" ,  "13" , "♭7" ,  "9" ]
      (FourWayClose, Chord.HalfDiminished) -> [  "1" , "♭3" ,  "♭5" , "♭7"        ]
      (FourWayClose, Chord.Altered)        -> [         "3" , "♭13" , "♭7" , "♯9" ]
      (FourWayClose, Chord.SevenFlat9)     -> [         "3" ,   "5" , "♭7" , "♭9" ]
      (FourWayClose, Chord.Sus)            -> [  "2" ,  "4" ,  "13" , "♭7"        ]

      _otherwise -> notYetImplemented (voicing, typ)

majorOtherDegree :: Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
majorOtherDegree voicing Chord.Name{root, typ} =
    consistentWith (voicing, typ) Scale.Major $
      case (voicing, root, typ) of
        -- Major 2-5-1 using four note close voicing
        --
        -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
        -- >  C    D    E    F    G    A    B   (C)   D  ( E)   F  ( G)   A  ( B)
        -- > ------------------------------------------------
        -- >       ._________*_________*_________*_________*                    iim7
        -- >                      ._________*______________*____*_________*     V7
        -- >                      ._________*_________*_________*_________*     V7(b9)
        -- >  ._________*_________*_________*_________*                         Imaj7
        (FourWayClose, "2", Chord.Minor7)     -> [ "4" ,  "6" ,  "8" ,  "10" ]
        (FourWayClose, "5", Chord.Dominant7)  -> [ "7" , "10" , "11" ,  "13" ]
        (FourWayClose, "5", Chord.SevenFlat9) -> [ "7" ,  "9" , "11" , "♭13" ]

        _otherwise -> notYetImplemented (voicing, root, typ)

{-------------------------------------------------------------------------------
  Scale degrees wrt minor scale

  We usually use a major scale as context, so we implement fewer cases here.
-------------------------------------------------------------------------------}

scaleDegreesMinor :: Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
scaleDegreesMinor voicing chordName
  | chordName.root == "1" = minorDegree1     voicing chordName.typ
  | otherwise             = minorOtherDegree voicing chordName

minorDegree1 :: Voicing -> Chord.Type -> NonEmpty Scale.Degree
minorDegree1 voicing typ =
    case (voicing, typ) of
      (Default      , Chord.Minor7)         -> [ "1" , "3" ,  "5" , "7"       ]
      (FourWayClose , Chord.Minor7)         -> [       "3" ,  "5" , "7" , "9" ]
      (FourWayClose , Chord.HalfDiminished) -> [ "1" , "3" , "♭5" , "7"       ]

      _otherwise -> notYetImplemented (voicing, typ)

minorOtherDegree :: Voicing -> Chord.Name Rel -> NonEmpty Scale.Degree
minorOtherDegree voicing Chord.Name{root, typ} =
    consistentWith (voicing, typ) Scale.Minor $
      case (voicing, root, typ) of
        -- Minor 2-5-1 using four note close voicing
        --
        -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
        -- >  C    D    E♭   F    G    A♭   B♭  (C)   D  (E♭)   F  ( G)  A♭  (B♭)
        -- > ------------------------------------------------
        -- >       *_________*_________*_________*                              iim7(b5)
        -- >                      ._________*_________*_________*_________*     V7(b9)
        -- >                      ._________*________ _____*____*_________*     V7(alt)
        -- >  ._________*_________*_________*_________*                         Imaj7
        --
        -- NOTE: The "Flat9" "SevenFlat9" does not refer to scale degrees, but
        -- rather to the interval between its nineth (second).
        --
        -- NOTE: In the context of the (natural) minor scale, we need to sharpen
        -- scale degree 7 to get the dominant chord (rather than a minor chord).
        (FourWayClose, "2", Chord.HalfDiminished) -> [  "2" ,  "4" ,  "6" ,   "8" ]
        (FourWayClose, "5", Chord.SevenFlat9)     -> [ "♯7" ,  "9" , "11" ,  "13" ]
        (FourWayClose, "5", Chord.Altered)        -> [ "♯7" , "10" , "11" , "𝄪13" ]

        _otherwise -> notYetImplemented (voicing, root, typ)

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

-- | Check that a particular voicing is consistent with the reference voicing
--
-- We cannot easily shift scale degrees, even if we know the underlying scale.
-- For example, should shifting degree "2" by a a semitone in a major scale
-- result in "♯2" or "♭3"? Function 'scaleDegrees' therefore treats voicings of
-- the same chord type, but starting at different scale degrees, as independent
-- cases. /However/, these voicings should still be /consistent/: no matter the
-- starting scale degree, the distance (in semitones) between the notes should
-- be the same.
--
-- We use the voicing on degree 1 of the major skill as our reference.
consistentWith ::
     HasCallStack
  => (Voicing, Chord.Type) -- ^ Reference
  -> Scale.Type            -- ^ Scale type of the scale degrees to verify
  -> NonEmpty Scale.Degree
  -> NonEmpty Scale.Degree
consistentWith reference@(voicing, typ) toVerifyScale toVerify
  | distances reference' == distances toVerify' = toVerify
  | otherwise = error $ "Consistency check failed for " ++ show reference
  where
    reference', toVerify' :: Unnamed.Chord Abs
    reference' = mkAbs Scale.Major   $ majorDegree1 voicing typ
    toVerify'  = mkAbs toVerifyScale $ toVerify

    -- Turn scale degrees into absolute notes
    --
    -- For the sake of this consistency check, we are only interested in the
    -- distance /between/ these notes; that means that we can pick an arbitrary
    -- scale root and arbitrary octave. (The reason we need to pick a scale roto
    -- at all is that this effects how the note are /spelled/, but that is not
    -- relevant here.)
    mkAbs :: Scale.Type -> NonEmpty Scale.Degree -> Unnamed.Chord Abs
    mkAbs scaleType degrees =
        Chord.Unnamed.wrtScale
          (Scale.named $ Scale.Name Scale.C scaleType)
          Octave.middle
          (Chord.Unnamed.fromScaleDegrees degrees)

    -- Distances between the notes in the chord
    distances :: Unnamed.Chord Abs -> [Word]
    distances = \(Unnamed.Chord (n :| ns)) -> go n ns
      where
        go :: Note.InOctave -> [Note.InOctave] -> [Word]
        go _ []      = []
        go n (n':ns) = distance n n' : go n' ns

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