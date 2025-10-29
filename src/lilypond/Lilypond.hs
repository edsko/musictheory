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
  , StaffElem(..)
  , Duration(..)
  ) where

import Data.Default

import MusicTheory.Chord.Named qualified as Named (Chord)
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
      title :: String
    , intro :: Maybe Ly.Markup
    , staff :: Staff -- TODO: Generalize to multiple staves
    }

data Staff = Staff{
      props :: StaffProps
    , elems :: [StaffElem]
    }

{-------------------------------------------------------------------------------
  Staff elements
-------------------------------------------------------------------------------}

data StaffProps = StaffProps{
      hideTimeSignature  :: Bool
    , omitMeasureNumbers :: Bool
    }
  deriving stock (Show)

instance Default StaffProps where
  def = StaffProps{
        hideTimeSignature  = False
      , omitMeasureNumbers = False
      }

data StaffElem =
    StaffNamedChord   (  Named.Chord Abs) Duration
  | StaffUnnamedChord (Unnamed.Chord Abs) Duration
  | StaffLinebreak
  | StaffComment String
  | StaffKeySignature Scale.Name
  deriving stock (Show)

data Duration =
    OneOver Word
  deriving stock (Show)
