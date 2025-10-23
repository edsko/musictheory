-- | Helpers for constructing Lilypond fragments
--
-- Intended for unqualified import.
module Exercises.Lilypond (
    -- * Titles
    sectionTitle
  , exerciseTitle
  ) where

import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Lilypond.Style qualified as Style

{-------------------------------------------------------------------------------
  Titles
-------------------------------------------------------------------------------}

sectionTitle :: String -> Ly.Markup Style.Class
sectionTitle = Ly.Markup.Style Style.SectionTitle . Ly.Markup.Quoted

exerciseTitle :: String -> Ly.Markup Style.Class
exerciseTitle = Ly.Markup.Style Style.ExerciseTitle . Ly.Markup.Quoted
