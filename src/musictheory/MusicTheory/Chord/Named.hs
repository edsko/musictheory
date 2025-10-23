{-# LANGUAGE OverloadedLists #-}

-- | Named chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Named qualified as Named (Chord)
-- > import MusicTheory.Chord.Named qualified as Chord.Named
module MusicTheory.Chord.Named (
    Chord -- opaque
    -- * Construction
  , relative
  , chordI
    -- * Query
  , getName
  , getType
  , getNotes
  ) where

import Data.Function
import Data.List.NonEmpty (NonEmpty)

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord)
import MusicTheory.Chord.Unnamed qualified as Unnamed.Chord
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Named chord
data Chord r where
  -- | Relative chord
  --
  -- Until we pick a scale, we cannot know which notes are in the chord,
  Rel :: Chord.Name Relative -> Chord Relative

  -- | Absolute
  Abs :: Chord.Name Absolute -> Unnamed.Chord Absolute -> Chord Absolute

deriving instance IsReferenceKind r => Show (Chord r)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

relative :: Chord.Name Relative -> Chord Relative
relative = Rel

-- | Chord of the specified type at the root of the scale
chordI :: Chord.Type -> Chord Relative
chordI typ = relative $ Chord.Name (Scale.Degree 1 Nothing) typ

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getName :: Chord r -> Chord.Name r
getName (Rel name)   = name
getName (Abs name _) = name

getType :: Chord r -> Chord.Type
getType = (.typ) . getName

getNotes :: Chord Absolute -> Unnamed.Chord Absolute
getNotes (Abs _ chord) = chord

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Invert (Chord Absolute) where
  invert = mapNotes . invert

instance TransposeOctave (Chord Absolute) where
  transposeOctave = mapNotes . transposeOctave

instance Distance (Chord Absolute) where
  distance = distance `on` getNotes

instance MakeAbsolute Chord where
  wrtScale octave scale (Rel name) =
      Abs name' $ wrtScale octave scale notes
    where
      name' :: Chord.Name Absolute
      name' = wrtScale octave scale name

      notes :: Unnamed.Chord Relative
      notes = Unnamed.Chord.fromScaleDegrees $
          scaleDegrees scale.name.typ name.typ name.root

{-------------------------------------------------------------------------------
  Expansion to scale degrees
-------------------------------------------------------------------------------}

scaleDegrees :: Scale.Type -> Chord.Type -> Scale.Degree -> NonEmpty Scale.Degree
scaleDegrees Scale.Major = scaleDegreesMajor
scaleDegrees Scale.Minor = scaleDegreesMinor

-- | Scale degrees for chord, wrt to the corresponding /major/ scale
--
-- > wrtMajorScale TriadMajor == [ "1" ,  "3" , "5" ]
-- > wrtMajorScale TriadMinor == [ "1" , "♭3" , "5" ]
scaleDegreesMajor :: Chord.Type -> Scale.Degree -> NonEmpty Scale.Degree

scaleDegreesMajor typ (Scale.Degree 1 Nothing) =
    case typ of
      --- Basic chords
      Chord.Basic_MajorTriad       -> [ "1" ,  "3" , "5"       ]
      Chord.Basic_MinorTriad       -> [ "1" , "♭3" , "5"       ]
      Chord.Basic_MajorSeventh     -> [ "1" ,  "3" , "5",  "7" ]
      Chord.Basic_MinorSeventh     -> [ "1" , "♭3" , "5", "♭7" ]
      Chord.Basic_DominantSeventh  -> [ "1" ,  "3" , "5", "♭7" ]

      -- Standard Jazz voicings
      Chord.StdJazz_Major          -> [ "3" ,  "5" ,  "7" ,  "9" ]
      Chord.StdJazz_Minor          -> [ "3" , "♭5" , "♭7" ,  "9" ]
      Chord.StdJazz_Dominant       -> [ "3" , "13" , "♭7" ,  "9" ]
      Chord.StdJazz_HalfDiminished -> [ "1" , "♭3" , "♭5" , "♭7" ]
      Chord.StdJazz_Altered        -> [ "3" , "♯5" , "♭7" , "♯9" ]
      Chord.StdJazz_AlteredFlat9   -> [ "3" ,  "5" , "♭7" , "♭9" ]

-- Chords with their root at different scale degrees
scaleDegreesMajor typ root =
    case (typ, root) of
      -- Major 2-5-1 using standard jazz voicing:
      --
      -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
      -- >  C    D    E    F    G    A    B   (C)   D  ( E)   F  ( G)   A  ( B)
      -- > ------------------------------------------------
      -- >       ._________*_________*_________*_________*                    (iim7)
      -- >                      ._________*_________*_________*_________*     (V7)
      -- >  ._________*_________*_________*_________*                         (Imaj7)
      (Chord.StdJazz_Minor    , "2") -> [ "4" , "6" ,  "8" , "10" ]
      (Chord.StdJazz_Dominant , "5") -> [ "7" , "9" , "11" , "13" ]

      -- We don't implement all possible combinations here
      _otherwise -> error $ "TODO: " ++ show (typ, root)

scaleDegreesMinor :: Chord.Type -> Scale.Degree -> NonEmpty Scale.Degree
scaleDegreesMinor typ root =
    case (typ, root) of
      -- Minor 2-5-1 using standard jazz voicing
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
      (Chord.StdJazz_HalfDiminished , "2") -> [  "2" , "4" ,  "6" ,  "8" ]
      (Chord.StdJazz_AlteredFlat9   , "5") -> [ "♯7" , "9" , "11" , "13" ]
      (Chord.StdJazz_Minor          , "1") -> [  "3" , "5" ,  "7" ,  "9" ]

      -- We don't implement all possible combinations here
      _otherwise -> error $ "TODO: " ++ show (typ, root)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Apply function to the notes of the chord, leaving the name unchanged
mapNotes ::
     (Unnamed.Chord Absolute -> Unnamed.Chord Absolute)
  -> Chord Absolute -> Chord Absolute
mapNotes f (Abs name chord) = Abs name (f chord)
