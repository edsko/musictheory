module MusicTheory where

import System.Random

import Util

{-------------------------------------------------------------------------------
  Basics
-------------------------------------------------------------------------------}

data Note =
    C
  | Cs
  | D
  | Ds
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | As
  | B
  deriving stock (Show, Eq, Ord, Enum, Bounded)

type Scale = [Note]

cMajor :: Scale
cMajor = [C, D, E, F, G, A, B]

{-------------------------------------------------------------------------------
  Chords
-------------------------------------------------------------------------------}

data Chord = Chord {
      -- | Chord number (1-indexed, to match musical conventions)
      chordNumber :: Int

      -- | Size of the chord (3 for a triad, 4 for a seventh chord, etc.)
    , chordArity :: Int
    }
  deriving stock (Show, Eq)

triad :: Int -> Chord
triad n = Chord n 3

seventh :: Int -> Chord
seventh n = Chord n 4

chord :: Scale -> Chord -> [Note]
chord scale (Chord n sz) = take sz . odds $ drop (n - 1) (cycle scale)

{-------------------------------------------------------------------------------
  Modes
-------------------------------------------------------------------------------}

modes :: Scale -> [Scale]
modes orig = [
      map (shift distance) rotated
    | n <- [0 .. 6]
    , let rotated  = rotate n orig
          distance = fromEnum (head orig) - fromEnum (head rotated)
    ]

data Mode =
    Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  deriving stock (Show, Eq, Ord, Enum, Bounded)

mode :: Mode -> Scale -> Scale
mode m scale = modes scale !! fromEnum m

{-------------------------------------------------------------------------------
  Random
-------------------------------------------------------------------------------}

randomChord :: IO Chord
randomChord = Chord <$> randomRIO (1, 7) <*> randomRIO (3, 4)
