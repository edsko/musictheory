-- | Render to Lilypond file
--
-- Intended for qualified import
--
-- > import Lilypond.Render qualified as Ly
module Lilypond.Render (render) where

import Prelude hiding (elem)

import Data.Char (toLower)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.String

import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Note (Note(..))
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale
import MusicTheory.Util.StringTable

import Lilypond (Lilypond)
import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup
import Lilypond.Render.Monad (RenderM)
import Lilypond.Render.Monad qualified as RenderM
import Lilypond.Util.Doc (Doc)
import Lilypond.Util.Doc qualified as Doc

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

class LilypondToDoc a where
  toDoc :: a -> RenderM Doc

render :: LilypondToDoc a => Ly.Markup.Stylesheet -> a -> String
render stylesheet =
    addFooter . Doc.render . RenderM.run stylesheet . toDoc
  where
    addFooter :: String -> String
    addFooter contents = contents ++ "\n% End lilypond\n"

{-------------------------------------------------------------------------------
  Render Lilypond fragments
-------------------------------------------------------------------------------}

instance LilypondToDoc Lilypond where
  toDoc lilypond = mconcat [
         "\\version \"2.24.3\""
       , "\\language \"english\""
      , RenderM.withinScope "layout" $ mconcat [
            assignReal "indent" 0.0
          ]
      , toDoc emptyHeader{tagline = Just ""}
      , pure RenderM.setupPaper
      , foldMap toDoc lilypond.books
      ]

instance LilypondToDoc Ly.Book where
  toDoc book = RenderM.withinScope "book" $ mconcat [
        toDoc emptyHeader{
            title    = Just $ fromString book.title
          , arranger = Just $ fromString book.author
          }
      , "\\markup \\vspace #2"
      , "\\markuplist \\table-of-contents"
      , "\\pageBreak"
      , foldMap toDoc book.parts
      ]

instance LilypondToDoc Ly.Bookpart where
  toDoc bookpart = RenderM.bookpart bookpart.title $ \tocLabel -> mconcat [
        toDoc emptyHeader{
            title    = Just $ Ly.Markup.Style (Ly.Markup.PartTitle tocLabel) $
                         fromString bookpart.title
          , arranger = Just ""
          }
      , foldMap toDoc bookpart.sections
      ]

instance LilypondToDoc Ly.Section where
  toDoc section = RenderM.section section.title $ \tocLabel -> mconcat [
        RenderM.markup $ Ly.Markup.Style (Ly.Markup.PartTitle tocLabel) $
          fromString section.title
      , RenderM.whenJust section.intro $ renderIntro
      , foldMap toDoc section.elems
      , "\\pageBreak"
      ]

instance LilypondToDoc Ly.SectionElem where
  toDoc = \case
      Ly.SectionScore score -> toDoc score
      Ly.SectionPageBreak   -> "\\pageBreak"
      Ly.SectionSub section -> toDoc section

instance LilypondToDoc Ly.Score where
  toDoc score = RenderM.score score.title $ \tocLabel -> mconcat [
        RenderM.markup $ Ly.Markup.Style (Ly.Markup.ScoreTitle tocLabel) $
          fromString score.title
      , RenderM.whenJust score.intro $ renderIntro
      , RenderM.withinScope "score" $
          toDoc score.staff
      ]

instance LilypondToDoc Ly.Staff where
  toDoc staff = mconcat [
        RenderM.withinScope "layout" $ mconcat [
            RenderM.withinScope "context" $
              RenderM.when staff.props.hideTimeSignature $
                -- <https://lilypond.org/doc/v2.24/Documentation/notation/visibility-of-objects>
                RenderM.line "\\Staff \\override TimeSignature.stencil = ##f"
          , RenderM.withinScope "context" $
              RenderM.when staff.props.omitMeasureNumbers $
                -- <https://lilypond.org/doc/v2.24/Documentation/snippets/rhythms#rhythms-removing-bar-numbers-from-a-score>
                RenderM.line "\\Score \\omit BarNumber"
          ]
      , RenderM.withinScope "new Staff" $ mconcat [
            toDoc staff.props.clef
          , toDoc staff.props.timeSignature
            -- Don't show key changes at the end of the line
            -- <https://lilypond.org/doc/v2.24/Documentation/notation/visibility-of-objects>
          , "\\set Staff.explicitKeySignatureVisibility = #end-of-line-invisible"
          , "\\set Staff.printKeyCancellation = ##f"
            -- Ensure chord annotations (above and below) are aligned
            -- <https://lilypond.org/doc/v2.24/Documentation/snippets/tweaks-and-overrides#tweaks-and-overrides-vertically-aligned-dynamics-and-textscripts>
          , "\\override TextScript.padding = #3"
          , RenderM.lines $ renderNotes staff.elems
          ]
      ]

instance LilypondToDoc Ly.Clef where
  toDoc Ly.ClefTreble = "\\clef treble"
  toDoc Ly.ClefBass   = "\\clef bass"

instance LilypondToDoc Ly.TimeSignature where
  toDoc (Ly.TimeSignature x y) = RenderM.line $ concat [
        "\\time "
      , show x
      , "/"
      , show y
      ]

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

-- | Header
data Header = Header{
      dedication  :: Maybe Ly.Markup
    , title       :: Maybe Ly.Markup
    , subtitle    :: Maybe Ly.Markup
    , subsubtitle :: Maybe Ly.Markup
    , instrument  :: Maybe Ly.Markup
    , poet        :: Maybe Ly.Markup
    , composer    :: Maybe Ly.Markup
    , meter       :: Maybe Ly.Markup
    , arranger    :: Maybe Ly.Markup
    , tagline     :: Maybe Ly.Markup
    , copyright   :: Maybe Ly.Markup
    , piece       :: Maybe Ly.Markup
    , opus        :: Maybe Ly.Markup
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

instance LilypondToDoc Header where
  toDoc header = RenderM.withinScope "header" $ mconcat [
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

renderNotes :: [Ly.StaffElem] -> [String]
renderNotes = \elems ->
    map aux elems
  where
    aux :: Ly.StaffElem -> String
    aux (Ly.StaffChord chord) = concat [
          renderUnnamed            chord.notes
        , renderDuration           chord.duration
        , maybe "" renderChordName chord.name
        , renderAnnotation         chord.annotation
        ]
    aux (Ly.StaffRest rest) = concat [
          "r" ++ renderDuration    rest.duration
        , maybe "" renderChordName rest.name
        , renderAnnotation         rest.annotation
        ]
    aux (Ly.StaffLinebreak) =
        "\\break"
    aux (Ly.StaffComment comment) =
        "% " ++ comment
    aux (Ly.StaffKeySignature scaleName) =
        renderKeySignature scaleName

renderAnnotation :: Ly.Annotation -> String
renderAnnotation Ly.NoAnnotation     = ""
renderAnnotation (Ly.Annotation ann) = concat [
      "_\\markup{\\italic{\\abs-fontsize #8 {"
    , ann
    , "}}}"
    ]

renderDuration :: Ly.Duration -> String
renderDuration (Ly.OneOver n) = show n

renderKeySignature :: Scale.Name -> String
renderKeySignature (Scale.Name root typ) = List.intercalate " " [
      "\\key"
    , case typ of
        Scale.Major -> renderNote (Scale.rootNote root) ++ " \\major"
        Scale.Minor -> renderNote (Scale.rootNote root) ++ " \\minor"
    ]

{-------------------------------------------------------------------------------
  'Note'
-------------------------------------------------------------------------------}

renderNote :: Note -> String
renderNote (Note name atal) = concat [
      renderNoteName name
    , maybe "" renderAccidental atal
    ]

renderNoteName :: Note.Name -> String
renderNoteName = map toLower . show

renderAccidental :: Note.Accidental -> [Char]
renderAccidental = \case
    Note.Sharp       -> "-sharp"
    Note.Flat        -> "-flat"
    Note.DoubleSharp -> "-sharpsharp"
    Note.DoubleFlat  -> "-flatflat"
    Note.Natural     -> "!"

{-------------------------------------------------------------------------------
  'Note.InOctave'
-------------------------------------------------------------------------------}

renderInOctave :: Note.InOctave -> String
renderInOctave (Note.InOctave o (Note.Note n ma)) = concat [
      renderNoteName n

      -- The syntax for accidentals is a bit strange: to force an accidental to
      -- be shown (independent from clef), the "!" must come /after/ the octave;
      -- this is the only way to explicitly show a \"natural\" accidental.
      -- However, other accidentals must come /before/ the octave.
    , case ma of
        Nothing -> renderOctave o
        Just a  ->
          case a of
            Note.Sharp       -> "-sharp"      ++ renderOctave o
            Note.Flat        -> "-flat"       ++ renderOctave o
            Note.DoubleSharp -> "-sharpsharp" ++ renderOctave o
            Note.DoubleFlat  -> "-flatflat"   ++ renderOctave o
            Note.Natural     ->                  renderOctave o ++ "!"
    ]

renderOctave :: Octave -> String
renderOctave o
  | o' < 0    = replicate (abs o') ','
  | o' > 0    = replicate      o'  '\''
  | otherwise = ""
  where
    -- Lilypond uses the octave /below/ middle C as the default
    o' = Octave.aboveMiddle o + 1

{-------------------------------------------------------------------------------
  Chords

  Although Lilypond has explicit support for chord names, it's not really that
  suited to our purpose: because it wants to be able to interpret the chords
  as notes, chord names must be chosen from a specific set, and it's not easy
  to change notation.
-------------------------------------------------------------------------------}

renderChordName :: Chord.Name Abs -> String
renderChordName Chord.Name{root = Note.InOctave _o note, typ} = concat [
      "^\\markup{"
    , stringTableEntry note
    , case typ of
        Chord.MajorTriad     -> ""
        Chord.MinorTriad     -> "m"
        Chord.Major7         -> sup "maj7"
        Chord.Minor7         -> "m" ++ sup "7"
        Chord.Dominant7      -> sup "7"
        Chord.HalfDiminished -> sup "ø"
        Chord.Altered        -> sup "alt"
        Chord.SevenFlat9     -> sup "7(♭9)"
        Chord.Sus            -> sup "sus"
    , "}"
    ]
  where
    sup :: String -> String
    sup text = "\\hspace #-0.5 \\normal-size-super{" ++ text ++ "}"

renderUnnamed :: Unnamed.Chord Abs -> String
renderUnnamed (Unnamed.Chord ns) =
    case ns of
      n :| []    -> renderInOctave n
      _otherwise -> concat [
          "<"
        , List.intercalate " " (map renderInOctave $ NE.toList ns)
        , ">"
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

assign :: String -> Ly.Markup -> RenderM Doc
assign var value = mconcat [
      fromString $ var ++ " ="
    , Doc.indent <$> RenderM.markup value
    ]

assignReal :: String -> Double -> RenderM Doc
assignReal var value = RenderM.line $ var ++ " = " ++ show value

renderIntro :: Ly.Markup.Markup -> RenderM Doc
renderIntro intro = mconcat [
      RenderM.markup intro
    , RenderM.lines [
          "\\markup \\center-column {"
        , "  \\vspace #1"
        , "}"
       ]
    ]
