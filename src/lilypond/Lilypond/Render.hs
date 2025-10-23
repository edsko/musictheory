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
import Data.String

import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Note (Note(..))
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

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
  toDoc :: a cls -> RenderM cls Doc

render :: LilypondToDoc a => Ly.Markup.IsClass cls -> a cls -> String
render isClass =
    addFooter . Doc.render . RenderM.run isClass . toDoc
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
      , pure RenderM.setupTableOfContents
      , foldMap toDoc lilypond.books
      ]

instance LilypondToDoc Ly.Book where
  toDoc book = RenderM.withinScope "book" $ mconcat [
        toDoc emptyHeader{
            title    = Just book.title
          , arranger = Just book.author
          }
      , "\\markup \\vspace #2"
      , "\\markuplist \\table-of-contents"
      , "\\pageBreak"
      , foldMap toDoc book.parts
      ]

instance LilypondToDoc Ly.Bookpart where
  toDoc bookPart = RenderM.bookPart bookPart.title $ mconcat [
        toDoc emptyHeader{
            title    = Just bookPart.title
          , arranger = Just ""
          }
      , foldMap toDoc bookPart.sections
      ]

instance LilypondToDoc Ly.Section where
  toDoc section = RenderM.section section.title $ mconcat [
        RenderM.markup section.title
      , RenderM.whenJust section.intro $ renderIntro
      , foldMap toDoc section.elems
      , "\\pageBreak"
      ]

instance LilypondToDoc Ly.SectionElem where
  toDoc = \case
      Ly.SectionScore score -> toDoc score
      Ly.SectionPageBreak   -> "\\pageBreak"

instance LilypondToDoc Ly.Score where
  toDoc score = mconcat [
        RenderM.markup score.title
      , RenderM.whenJust score.intro $ renderIntro
      , RenderM.score score.title $
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
      , "<<"
      , RenderM.indent $ mconcat [
            RenderM.withinScope "chords" $ mconcat [
                RenderM.line $ "\\set noChordSymbol = \"\""
              , RenderM.lines $ renderChordNames staff.elems
              ]
          , RenderM.withinScope "new Staff" $ mconcat [
                -- Don't show key changes at the end of the line
                -- <https://lilypond.org/doc/v2.24/Documentation/notation/visibility-of-objects>
                "\\set Staff.explicitKeySignatureVisibility = #end-of-line-invisible"
              , "\\set Staff.printKeyCancellation = ##f"
              , RenderM.lines $ renderNotes staff.elems
              ]
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
data Header cls = Header{
      dedication  :: Maybe (Ly.Markup cls)
    , title       :: Maybe (Ly.Markup cls)
    , subtitle    :: Maybe (Ly.Markup cls)
    , subsubtitle :: Maybe (Ly.Markup cls)
    , instrument  :: Maybe (Ly.Markup cls)
    , poet        :: Maybe (Ly.Markup cls)
    , composer    :: Maybe (Ly.Markup cls)
    , meter       :: Maybe (Ly.Markup cls)
    , arranger    :: Maybe (Ly.Markup cls)
    , tagline     :: Maybe (Ly.Markup cls)
    , copyright   :: Maybe (Ly.Markup cls)
    , piece       :: Maybe (Ly.Markup cls)
    , opus        :: Maybe (Ly.Markup cls)
    }

emptyHeader :: Header cls
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

renderChordNames :: [Ly.StaffElem] -> [String]
renderChordNames = \elems ->
    map aux elems
  where
    aux :: Ly.StaffElem -> String
    aux (Ly.StaffNamedChord chord duration) =
        renderChordName (Chord.Named.getName chord) duration
    aux (Ly.StaffUnnamedChord _chord duration) =
        "r" ++ renderDuration duration -- No chord name
    aux (Ly.StaffLinebreak) =
        "" -- We generate the linebreak when rendering the staff itself
    aux (Ly.StaffComment comment) =
        "% " ++ comment
    aux (Ly.StaffKeySignature scaleName) =
        "% " ++ show scaleName

renderNotes :: [Ly.StaffElem] -> [String]
renderNotes = \elems ->
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
    aux (Ly.StaffComment comment) =
        "% " ++ comment
    aux (Ly.StaffKeySignature scaleName) =
        renderKeySignature scaleName

renderDuration :: Ly.Duration -> String
renderDuration (Ly.OneOver n) = show n

renderKeySignature :: Scale.Name -> String
renderKeySignature (Scale.Name root typ) = intercalate " " [
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
-------------------------------------------------------------------------------}

renderChordName :: Chord.Name Absolute -> Ly.Duration -> String
renderChordName (Chord.Name (Note.InOctave _o note) typ) d =
    concat [
        renderNote note
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
    Chord.StdJazz_AlteredFlat9   -> ":3.5.7.9-"

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
  Internal auxiliary
-------------------------------------------------------------------------------}

assign :: String -> Ly.Markup cls -> RenderM cls Doc
assign var value = mconcat [
      fromString $ var ++ " ="
    , Doc.indent <$> RenderM.markup value
    ]

assignReal :: String -> Double -> RenderM cls Doc
assignReal var value = RenderM.line $ var ++ " = " ++ show value

renderIntro :: Ly.Markup.Markup cls -> RenderM cls Doc
renderIntro intro = mconcat [
      RenderM.markup intro
    , RenderM.lines [
          "\\markup \\center-column {"
        , "  \\vspace #1"
        , "}"
       ]
    ]
