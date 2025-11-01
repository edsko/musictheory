-- | Lilypond styling
module Exercises.Lilypond.Style (stylesheet) where

import Data.String

import Lilypond.Markup qualified as Ly.Markup
import Lilypond.Part qualified as Ly.Part
import Lilypond.Util.Doc (Doc)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

stylesheet :: Ly.Markup.Stylesheet
stylesheet = \case
    Ly.Markup.PartTitle  label -> styleTitle label
    Ly.Markup.ScoreTitle label -> styleScore label

{-------------------------------------------------------------------------------
  Interpretation
-------------------------------------------------------------------------------}

styleTitle :: Ly.Part.Label -> Doc -> Doc
styleTitle label =
    case length (Ly.Part.index label) of
      1 -> styleBookpart   label
      2 -> styleSection    label
      3 -> styleSubsection label
      _ -> error $ "Unexpected label " ++ show label

-- | Book parts
--
-- Here lilypond already applies its own formatting, so we just add the labe.
styleBookpart :: Ly.Part.Label -> Doc -> Doc
styleBookpart = titleWithLabel

styleSection :: Ly.Part.Label -> Doc -> Doc
styleSection label title =
    Ly.Markup.withinScope "center-column" $ mconcat [
        "\\vspace #1"
      , Ly.Markup.withinScope "abs-fontsize #16" $
          Ly.Markup.withinScope "italic" $
              titleWithLabel label title
      , "\\vspace #1"
      ]

styleSubsection :: Ly.Part.Label -> Doc -> Doc
styleSubsection label title =
    Ly.Markup.withinScope "center-column" $ mconcat [
        "\\vspace #0.5"
      , Ly.Markup.withinScope "abs-fontsize #13" $
          titleWithLabel label title
      , "\\vspace #0.5"
      ]

styleScore :: Ly.Part.Label -> Doc -> Doc
styleScore _label title =
    Ly.Markup.withinScope "center-column" $ mconcat [
        "\\vspace #0.5"
      , Ly.Markup.withinScope "bold" $
          titleWithLabel _label title
      , "\\vspace #0.5"
      ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

titleWithLabel :: Ly.Part.Label -> Doc -> Doc
titleWithLabel label title =
    Ly.Markup.withinScope "wordwrap" $ mconcat [
        fromString $ Ly.Part.render label ++ ". "
      , title
      ]