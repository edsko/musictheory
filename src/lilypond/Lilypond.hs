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

data Lilypond cls = Lilypond{
      books  :: [Book cls]
    }

data Book cls = Book{
      title  :: Ly.Markup cls
    , author :: Ly.Markup cls
    , parts  :: [Bookpart cls]
    }

data Bookpart cls = Bookpart{
      title    :: Ly.Markup cls
    , sections :: [Section cls]
    }

data Section cls = Section{
      title  :: Ly.Markup cls
    , intro  :: Maybe (Ly.Markup cls)
    , elems  :: [SectionElem cls]
    }

data SectionElem cls =
    SectionScore (Score cls)
  | SectionPageBreak

data Score cls = Score{
      title :: Ly.Markup cls
    , intro :: Maybe (Ly.Markup cls)
    , staff :: Staff cls -- TODO: Generalize to multiple staves
    }

data Staff cls = Staff{
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
