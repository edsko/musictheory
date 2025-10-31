-- | Markup
--
-- Intended for qualified import.
--
-- > import Lilypond.Markup qualified as Ly (Markup)
-- > import Lilypond.Markup qualified as Ly.Markup
module Lilypond.Markup (
    Markup(..)
  , Music(..)
  , Scope(..)
  , render
    -- * Convenience constructors
  , wordwrap
  , italic
  , fontsize
    -- * Styling
  , Style(..)
  , Stylesheet
    -- * Auxiliary
  , withinScope
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup
import Data.String

import MusicTheory.Interval (Interval)

import Lilypond.Part qualified as Ly.Part
import Lilypond.Util.Doc (Doc)
import Lilypond.Util.Doc qualified as Doc

{-------------------------------------------------------------------------------
  Markup
-------------------------------------------------------------------------------}

-- | Markup
data Markup =
    -- | Empty
    Empty

    -- | Plain text (support 'IsString')
  | Text String

    -- | Quoted text
  | Quoted String

    -- | Music element
    --
    -- <https://lilypond.org/doc/v2.24/Documentation/notation/music>
  | Music Music

    -- | Scoped environment
  | Scope Scope Markup

    -- | Support 'Semigroup' and 'Monoid'
  | MConcat [Markup]

    -- | Apply style class
  | Style Style Markup

data Music =
    Interval Interval

data Scope =
    Wordwrap
  | Italic
  | Fontsize Word

instance IsString  Markup where fromString = Text
instance Monoid    Markup where mconcat    = MConcat
instance Semigroup Markup where sconcat    = MConcat . NonEmpty.toList

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

wordwrap :: Markup -> Markup
wordwrap = Scope Wordwrap

italic :: Markup -> Markup
italic = Scope Italic

fontsize :: Word -> Markup -> Markup
fontsize = Scope . Fontsize

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

-- | Styling of individual elements
data Style =
    -- | Book part or (sub)section title
    PartTitle Ly.Part.Label

    -- | Exercise title
    --
    -- These are distinguished from general 'PartTitle', so we can use
    -- consistent styling no matter the nesting depth.
  | ScoreTitle Ly.Part.Label

-- | How to interpret styles
type Stylesheet = Style -> Doc -> Doc

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

render :: Stylesheet -> Markup -> Doc
render stylesheet = withinScope "markup" . go
  where
    go :: Markup -> Doc
    go Empty             = mempty
    go (Text     str)    = Doc.line $ str
    go (Quoted   str)    = Doc.line $ "\"" ++ str ++ "\""
    go (Music    music)  = goMusic music
    go (Scope s  markup) = withinScope (scope s) $ go markup
    go (MConcat  markup) = foldMap go markup
    go (Style c  markup) = stylesheet c $ go markup

    -- Our unicode rendering works just fine in Lilypond
    goMusic :: Music -> Doc
    goMusic (Interval atal) = Doc.line $ show atal

    scope :: Scope -> String
    scope Wordwrap      = "wordwrap"
    scope Italic        = "italic"
    scope (Fontsize sz) = "abs-fontsize #" ++ show sz

withinScope :: String -> Doc -> Doc
withinScope label body = mconcat [
      Doc.line $ "\\" ++ label ++ " {"
    , Doc.indent body
    , "}"
    ]
