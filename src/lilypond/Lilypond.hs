-- | Basic Lilypond structure
--
-- > import Lilypond (Lilypond)
-- > import Lilypond qualified as Ly
module Lilypond (
    -- * Document structure
    Lilypond(..)
  , Book(..)
  , Bookpart(..)
  , Section(..)
  , SectionElem(..)
  , Score(..)
  , Staff(..)
    -- * Staff elements
  , StaffProps(..)
  , TimeSignature(..)
  , Clef(..)
  , StaffElem(..)
  , Chord(..)
  , Rest(..)
  , Annotation(..)
  , Duration(..)
  ) where

import Data.Default
import Data.String

import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

import Lilypond.Markup qualified as Ly (Markup)

{-------------------------------------------------------------------------------
  Document structure
-------------------------------------------------------------------------------}

data Lilypond = Lilypond{
      books  :: [Book]
    }

data Book  = Book{
      title  :: String
    , author :: String
    , parts  :: [Bookpart]
    }

data Bookpart = Bookpart{
      title    :: String
    , sections :: [Section]
    }

data Section = Section{
      title  :: String
    , intro  :: Maybe Ly.Markup
    , elems  :: [SectionElem]
    }

data SectionElem =
    SectionScore Score
  | SectionPageBreak
  | SectionSub Section

data Score = Score{
      title :: Maybe String
    , intro :: Maybe Ly.Markup
    , staff :: Staff -- There can only be one
    }

data Staff = Staff{
      props :: StaffProps
    , elems :: [StaffElem]
    }

{-------------------------------------------------------------------------------
  Staff elements
-------------------------------------------------------------------------------}

data StaffProps = StaffProps{
      clef               :: Clef
    , timeSignature      :: TimeSignature
    , hideTimeSignature  :: Bool
    , omitMeasureNumbers :: Bool
    }
  deriving stock (Show)

data Clef =
    ClefTreble
  | ClefBass
  deriving stock (Show)

data TimeSignature = TimeSignature Int Int
  deriving stock (Show)

instance Default StaffProps where
  def = StaffProps{
        clef               = ClefTreble
      , timeSignature      = TimeSignature 4 4
      , hideTimeSignature  = False
      , omitMeasureNumbers = False
      }

data StaffElem =
    StaffChord Chord
  | StaffRest Rest
  | StaffLinebreak
  | StaffComment String
  | StaffKeySignature Scale.Name

data Chord = Chord{
      notes      :: Unnamed.Chord Abs
    , duration   :: Duration
    , name       :: Maybe (Chord.Name Abs)
    , annotation :: Annotation
    }

data Rest = Rest{
      duration   :: Duration
    , name       :: Maybe (Chord.Name Abs)
    , annotation :: Annotation
    }

data Annotation =
    NoAnnotation
  | Annotation String

instance IsString Annotation where
  fromString = Annotation

data Duration =
    OneOver Word
  deriving stock (Show)
