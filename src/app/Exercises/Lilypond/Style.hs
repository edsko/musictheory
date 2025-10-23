-- | Lilypond styling
--
-- > import Exercises.Lilypond.Style qualified as Style
module Exercises.Lilypond.Style (
    Class(..)
  , isClass
  ) where

import Lilypond.Markup qualified as Ly.Markup
import Lilypond.Util.Doc (Doc)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Class =
    ExerciseTitle
  | SectionTitle

{-------------------------------------------------------------------------------
  Interpretation
-------------------------------------------------------------------------------}

isClass :: Ly.Markup.IsClass Class
isClass = Ly.Markup.IsClass $ \case
    SectionTitle  -> Ly.Markup.Wrap sectionTitle
    ExerciseTitle -> Ly.Markup.Wrap exerciseTitle

sectionTitle :: Doc -> Doc
sectionTitle title =
    Ly.Markup.withinScope "center-column" $ mconcat [
        "\\vspace #1"
      , Ly.Markup.withinScope "abs-fontsize #16" $
          Ly.Markup.withinScope "italic" title
      , "\\vspace #1"
      ]

exerciseTitle :: Doc -> Doc
exerciseTitle title =
    Ly.Markup.withinScope "center-column" $ mconcat [
        "\\vspace #0.5"
      , Ly.Markup.withinScope "bold" title
      , "\\vspace #0.5"
      ]
