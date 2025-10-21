-- | Render to Lilypond file
--
-- Intended for qualified import
--
-- > import Lilypond.Render qualified as Ly
module Lilypond.Render (render) where

import Prelude hiding (elem)

import Data.Char (toLower)
import Data.List (intercalate)

import MusicTheory.Chord qualified as Chord
import MusicTheory.Note qualified as Note

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
      , scope "layout" $ mconcat [
            assignReal "indent" 0.0
          ]
      , toDoc emptyHeader{tagline = Just ""}
      , pure RenderM.setupTableOfContents
      , foldMap toDoc lilypond.books
      ]

instance ToDoc Ly.Book where
  toDoc book = scope "book" $ mconcat [
        toDoc emptyHeader{
            title    = Just book.title
          , arranger = Just book.author
          }
      , "\\markuplist \\table-of-contents"
      , "\\pageBreak"
      , foldMap toDoc book.parts
      ]

instance ToDoc Ly.Bookpart where
  toDoc bookPart = RenderM.bookPart bookPart.title $
      scope "bookpart" $ mconcat [
          toDoc emptyHeader{title = Just bookPart.title}
        , foldMap toDoc bookPart.sections
        ]

instance ToDoc Ly.Section where
  toDoc section = RenderM.section section.title $ mconcat [
        RenderM.lines [
            "  \\markup \\center-column {"
          , "  \\vspace #1"
          , "  \\abs-fontsize #16 \\italic \"" ++ section.title ++ "\""
          , "  \\vspace #1"
          , "}"
          ]
      , foldMap toDoc section.scores
      ]

instance ToDoc Ly.Score where
  toDoc score = mconcat [
        RenderM.score score.title
      , scope "score" $ mconcat [
          toDoc emptyHeader{piece = Just score.title}
        , toDoc score.elems
        ]
      ]

instance ToDoc Ly.ScoreElem where
  toDoc (Ly.ScoreStaff props content) = mconcat [
        scope "layout" $ mconcat [
            scope "context" $
              RenderM.when props.hideTimeSignature $
                -- <https://lilypond.org/doc/v2.24/Documentation/notation/visibility-of-objects>
                RenderM.line "\\Staff \\override TimeSignature.stencil = ##f"
          , scope "context" $
              RenderM.when props.omitMeasureNumbers $
                -- <https://lilypond.org/doc/v2.24/Documentation/snippets/rhythms#rhythms-removing-bar-numbers-from-a-score>
                RenderM.line "\\Score \\omit BarNumber"
          ]
      , "<<"
      , RenderM.indent $ mconcat [
            scope "chords" $
              RenderM.line $ renderChordNames content
          , scope "new Staff" $
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
  toDoc header = scope "header" $ mconcat [
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
    aux :: Ly.StaffElem -> [Char]
    aux (Ly.StaffChord chord) =
        case chord.name of
          Nothing   -> error "TODO" -- should be a rest
          Just name -> renderChordName name chord.duration
    aux (Ly.StaffLinebreak) =
        -- We generate the linebreak when rendering the staff itself
        ""

renderNotes :: [Ly.StaffElem] -> String
renderNotes = \elems -> intercalate " " $
    map aux elems
  where
    aux :: Ly.StaffElem -> String
    aux (Ly.StaffChord chord) = concat [
          renderChord    chord.notes
        , renderDuration chord.duration
        ]
    aux (Ly.StaffLinebreak) =
        "\\break"

renderDuration :: Ly.Duration -> String
renderDuration = \case
    Ly.Whole        -> "1"
    Ly.Half         -> "2"
    Ly.Quarter      -> "4"
    Ly.Eighth       -> "8"
    Ly.Sixteenth    -> "16"
    Ly.Thirtysecond -> "32"

{-------------------------------------------------------------------------------
  Chords
-------------------------------------------------------------------------------}

renderChordName :: Chord.Name -> Ly.Duration -> String
renderChordName (Chord.Name note typ) d = concat [
      renderSimpleNote note
    , renderDuration d
    , renderChordType typ
    ]

-- <https://lilypond.org/doc/v2.24/Documentation/notation/common-chord-modifiers>
renderChordType :: Chord.Type -> String
renderChordType = \case
    Chord.MajorTriad      -> ""
    Chord.MinorTriad      -> ":m"
    Chord.MajorSeventh    -> ":maj7"
    Chord.MinorSeventh    -> ":m7"
    Chord.DominantSeventh -> ":7"

renderChord :: Chord.Chord -> String
renderChord (Chord.Chord ns) =
    case ns of
      [n] -> renderInOctave n
      _   -> "<" ++ intercalate " " (map renderInOctave ns) ++ ">"

{-------------------------------------------------------------------------------
  Simple notes
-------------------------------------------------------------------------------}

renderSimpleNote :: Note.Simple -> String
renderSimpleNote (Note.Simple n ma) = concat [
      renderNoteName n
    , renderSimpleAccidental ma
    ]

renderNoteName :: Note.Name -> String
renderNoteName = map toLower . show

renderSimpleAccidental :: Maybe Note.SimpleAccidental -> String
renderSimpleAccidental Nothing  = ""
renderSimpleAccidental (Just a) =
    case a of
      Note.SimpleSharp -> "-sharp"
      Note.SimpleFlat  -> "-flat"

{-------------------------------------------------------------------------------
  Notes
-------------------------------------------------------------------------------}

renderInOctave :: Note.InOctave -> String
renderInOctave (Note.InOctave (Note.Note n ma) o) = concat [
      renderNoteName n
    , renderAccidental ma o
    ]

-- The syntax for accidentals is a bit strange: to force an accidental to
-- be shown (independent from clef), the "!" must come /after/ the octave;
-- this is the only way to explicitly show a \"natural\" accidental.
-- However, other accidentals must come /before/ the octave.
renderAccidental :: Maybe Note.Accidental -> Note.Octave -> String
renderAccidental Nothing  o = renderOctave o
renderAccidental (Just a) o =
    case a of
      Note.Sharp       -> "-sharp"      ++ renderOctave o
      Note.Flat        -> "-flat"       ++ renderOctave o
      Note.DoubleSharp -> "-sharpsharp" ++ renderOctave o
      Note.DoubleFlat  -> "-flatflat"   ++ renderOctave o
      Note.Natural     ->                  renderOctave o ++ "!"

renderOctave :: Note.Octave -> String
renderOctave o
  | o' < 0    = replicate (abs o') ','
  | o' > 0    = replicate      o'  '\''
  | otherwise = ""
  where
    -- Lilypond uses the octave /below/ middle C as the default
    o' = Note.aboveMiddleOctave o + 1

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

scope :: String -> RenderM Doc -> RenderM Doc
scope name body = mconcat [
      RenderM.line $ "\\" ++ name ++ "{"
    , RenderM.indent body
    , "}"
    ]

assign :: String -> String -> RenderM Doc
assign var value = RenderM.line $ var ++ " = \"" ++ value ++ "\""

assignReal :: String -> Double -> RenderM Doc
assignReal var value = RenderM.line $ var ++ " = " ++ show value