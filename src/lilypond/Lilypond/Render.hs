-- | Render to Lilypond file
--
-- Intended for qualified import
--
-- > import Lilypond.Render qualified as Ly
module Lilypond.Render (render) where

import Prelude hiding (elem)

import Data.Char (toLower)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE

import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Reference

import Lilypond (Lilypond)
import Lilypond qualified as Ly
import Lilypond.Render.Monad (RenderM)
import Lilypond.Render.Monad qualified as RenderM
import Lilypond.Util.Pretty (Doc)
import Lilypond.Util.Pretty qualified as Doc

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

class ToDoc a where
  toDoc :: a -> RenderM Doc

render :: ToDoc a => a -> String
render = addFooter . Doc.render . RenderM.run . toDoc
  where
    addFooter :: String -> String
    addFooter contents = contents ++ "\n% End lilypond\n"

{-------------------------------------------------------------------------------
  Render Lilypond fragments
-------------------------------------------------------------------------------}

instance ToDoc Lilypond where
  toDoc lilypond = mconcat [
         "\\version \"2.24.3\""
       , "\\language \"english\""
      , RenderM.scope "layout" $ mconcat [
            assignReal "indent" 0.0
          ]
      , toDoc emptyHeader{tagline = Just ""}
      , pure RenderM.setupTableOfContents
      , foldMap toDoc lilypond.books
      ]

instance ToDoc Ly.Book where
  toDoc book = RenderM.scope "book" $ mconcat [
        toDoc emptyHeader{
            title    = Just book.title
          , arranger = Just book.author
          }
      , "\\markup \\vspace #2"
      , "\\markuplist \\table-of-contents"
      , "\\pageBreak"
      , foldMap toDoc book.parts
      ]

instance ToDoc Ly.Bookpart where
  toDoc bookPart = RenderM.bookPart bookPart.title $ mconcat [
        toDoc emptyHeader{
            title    = Just bookPart.title
          , arranger = Just ""
          }
      , foldMap toDoc bookPart.sections
      ]

instance ToDoc Ly.Section where
  toDoc section = RenderM.section section.title $ mconcat [
        RenderM.lines [
            "\\markup \\center-column {"
          , "  \\vspace #1"
          , "  \\abs-fontsize #16 \\italic \"" ++ section.title ++ "\""
          , "  \\vspace #1"
          , "}"
          ]
      , RenderM.whenJust section.intro $ \intro -> RenderM.lines [
            "\\markup \\wordwrap {"
          , "  " ++ intro
          , "}"
          , "\\markup \\center-column {"
          , "  \\vspace #1"
          , "}"
          ]
      , foldMap toDoc section.scores
      , "\\pageBreak"
      ]

instance ToDoc Ly.Score where
  toDoc score = RenderM.score score.title $ mconcat [
        toDoc emptyHeader{piece = Just score.title}
      , toDoc score.elems
      ]

instance ToDoc Ly.ScoreElem where
  toDoc (Ly.ScoreStaff props content) = mconcat [
        RenderM.scope "layout" $ mconcat [
            RenderM.scope "context" $
              RenderM.when props.hideTimeSignature $
                -- <https://lilypond.org/doc/v2.24/Documentation/notation/visibility-of-objects>
                RenderM.line "\\Staff \\override TimeSignature.stencil = ##f"
          , RenderM.scope "context" $
              RenderM.when props.omitMeasureNumbers $
                -- <https://lilypond.org/doc/v2.24/Documentation/snippets/rhythms#rhythms-removing-bar-numbers-from-a-score>
                RenderM.line "\\Score \\omit BarNumber"
          ]
      , "<<"
      , RenderM.indent $ mconcat [
            RenderM.scope "chords" $ mconcat [
                RenderM.line $ "\\set noChordSymbol = \"\""
              , RenderM.line $ renderChordNames content
              ]
          , RenderM.scope "new Staff" $
              RenderM.line $ renderNotes content
          ]
      , ">>"
      ]

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

-- | Header
--
-- TODO: These should not be strings: they support markup.
-- <https://lilypond.org/doc/v2.24/Documentation/notation/creating-titles-headers-and-footers#default-layout-of-Bookpart-and-score-titles>
data Header = Header{
      dedication  :: Maybe String
    , title       :: Maybe String
    , subtitle    :: Maybe String
    , subsubtitle :: Maybe String
    , instrument  :: Maybe String
    , poet        :: Maybe String
    , composer    :: Maybe String
    , meter       :: Maybe String
    , arranger    :: Maybe String
    , tagline     :: Maybe String
    , copyright   :: Maybe String
    , piece       :: Maybe String
    , opus        :: Maybe String
    }

emptyHeader :: Header
emptyHeader = Header{
      dedication  = Nothing
    , title       = Nothing
    , subtitle    = Nothing
    , subsubtitle = Nothing
    , instrument  = Nothing
    , poet        = Nothing
    , composer    = Nothing
    , meter       = Nothing
    , arranger    = Nothing
    , tagline     = Nothing
    , copyright   = Nothing
    , piece       = Nothing
    , opus        = Nothing
    }

instance ToDoc Header where
  toDoc header = RenderM.scope "header" $ mconcat [
        RenderM.whenJust header.dedication  $ assign "dedication"
      , RenderM.whenJust header.title       $ assign "title"
      , RenderM.whenJust header.subtitle    $ assign "subtitle"
      , RenderM.whenJust header.subsubtitle $ assign "subsubtitle"
      , RenderM.whenJust header.instrument  $ assign "instrument"
      , RenderM.whenJust header.poet        $ assign "poet"
      , RenderM.whenJust header.composer    $ assign "composer"
      , RenderM.whenJust header.meter       $ assign "meter"
      , RenderM.whenJust header.arranger    $ assign "arranger"
      , RenderM.whenJust header.tagline     $ assign "tagline"
      , RenderM.whenJust header.copyright   $ assign "copyright"
      , RenderM.whenJust header.piece       $ assign "piece"
      , RenderM.whenJust header.opus        $ assign "opus"
      ]

{-------------------------------------------------------------------------------
  Render a single 'Staff'
-------------------------------------------------------------------------------}

renderChordNames :: [Ly.StaffElem] -> String
renderChordNames = \elems ->
    intercalate " " $ map aux elems
  where
    aux :: Ly.StaffElem -> String
    aux (Ly.StaffNamedChord chord duration) =
        renderChordName (Chord.Named.getName chord) duration
    aux (Ly.StaffUnnamedChord _chord duration) =
        "r" ++ renderDuration duration -- No chord name
    aux (Ly.StaffLinebreak) =
        "" -- We generate the linebreak when rendering the staff itself

renderNotes :: [Ly.StaffElem] -> String
renderNotes = \elems -> intercalate " " $
    map aux elems
  where
    aux :: Ly.StaffElem -> String
    aux (Ly.StaffNamedChord chord duration) = concat [
          renderUnnamed  (Chord.Named.getNotes chord)
        , renderDuration duration
        ]
    aux (Ly.StaffUnnamedChord chord duration) = concat [
          renderUnnamed  chord
        , renderDuration duration
        ]
    aux (Ly.StaffLinebreak) =
        "\\break"

renderDuration :: Ly.Duration -> String
renderDuration (Ly.OneOver n) = show n

{-------------------------------------------------------------------------------
  Chords
-------------------------------------------------------------------------------}

renderChordName :: Chord.Name Absolute -> Ly.Duration -> String
renderChordName (Chord.Name (Note.InOctave _o (Note.Note n a)) typ) d =
    concat [
        renderNoteName n
      , case a of
          Nothing               -> ""
          Just Note.Sharp       -> "-sharp"
          Just Note.Flat        -> "-flat"
          Just Note.DoubleSharp -> "-sharpsharp"
          Just Note.DoubleFlat  -> "-flatflat"
          Just Note.Natural     -> "!"
      , renderDuration d
      , renderChordType typ
      ]

-- Render chord type
--
-- <https://lilypond.org/doc/v2.24/Documentation/notation/common-chord-modifiers>
--
-- TODO: Ideally we'd render the altered chord as "alt", but haven't managed to
-- do that so far.
renderChordType :: Chord.Type -> String
renderChordType = \case
    Chord.Basic_MajorTriad        -> ""
    Chord.Basic_MinorTriad        -> ":m"
    Chord.Basic_MajorSeventh      -> ":maj7"
    Chord.Basic_MinorSeventh      -> ":m7"
    Chord.Basic_DominantSeventh   -> ":7"

    Chord.StdJazz_Major          -> ":maj7"
    Chord.StdJazz_Minor          -> ":m7"
    Chord.StdJazz_Dominant       -> ":7"
    Chord.StdJazz_HalfDiminished -> ":m7.5-"
    Chord.StdJazz_Altered        -> ":3.5+.7.9+"

renderUnnamed :: Unnamed.Chord Absolute -> String
renderUnnamed (Unnamed.Chord ns) =
    case ns of
      n :| []    -> renderInOctave n
      _otherwise -> concat [
          "<"
        , intercalate " " (map renderInOctave $ NE.toList ns)
        , ">"
        ]

{-------------------------------------------------------------------------------
  Notes
-------------------------------------------------------------------------------}

renderInOctave :: Note.InOctave -> String
renderInOctave (Note.InOctave o (Note.Note n ma)) = concat [
      renderNoteName n
    , renderAccidental ma o
    ]

renderNoteName :: Note.Name -> String
renderNoteName = map toLower . show

-- The syntax for accidentals is a bit strange: to force an accidental to
-- be shown (independent from clef), the "!" must come /after/ the octave;
-- this is the only way to explicitly show a \"natural\" accidental.
-- However, other accidentals must come /before/ the octave.
renderAccidental :: Maybe Note.Accidental -> Octave -> String
renderAccidental Nothing  o = renderOctave o
renderAccidental (Just a) o =
    case a of
      Note.Sharp       -> "-sharp"      ++ renderOctave o
      Note.Flat        -> "-flat"       ++ renderOctave o
      Note.DoubleSharp -> "-sharpsharp" ++ renderOctave o
      Note.DoubleFlat  -> "-flatflat"   ++ renderOctave o
      Note.Natural     ->                  renderOctave o ++ "!"

renderOctave :: Octave -> String
renderOctave o
  | o' < 0    = replicate (abs o') ','
  | o' > 0    = replicate      o'  '\''
  | otherwise = ""
  where
    -- Lilypond uses the octave /below/ middle C as the default
    o' = Octave.aboveMiddle o + 1

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

assign :: String -> String -> RenderM Doc
assign var value = RenderM.line $ var ++ " = \"" ++ value ++ "\""

assignReal :: String -> Double -> RenderM Doc
assignReal var value = RenderM.line $ var ++ " = " ++ show value