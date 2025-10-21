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
import Lilypond.Util.Pretty (Doc)
import Lilypond.Util.Pretty qualified as Doc

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

render :: ToDoc a => a -> String
render = addFooter . Doc.render . toDoc
  where
    addFooter :: String -> String
    addFooter contents = contents ++ "\n% End lilypond\n"

{-------------------------------------------------------------------------------
  Internal: 'ToDoc' class
-------------------------------------------------------------------------------}

class ToDoc a where
  toDoc :: a -> Doc

{-------------------------------------------------------------------------------
  Render Lilypond fragments
-------------------------------------------------------------------------------}

instance ToDoc Lilypond where
  toDoc lilypond = mconcat [
        "\\version \"2.24.3\""
      , "\\language \"english\""
      , section "layout" $ mconcat [
            assignReal "indent" 0.0
          ]
      , toDoc lilypond.header
      , foldMap toDoc lilypond.books
      ]

instance ToDoc Ly.Book where
  toDoc book = section "book" $ mconcat [
        toDoc book.header
      , "\\pageBreak"
      , foldMap toDoc book.parts
      ]

instance ToDoc Ly.BookPart where
  toDoc bookPart = section "bookpart" $ mconcat [
        toDoc bookPart.header
      , foldMap toDoc bookPart.elems
      ]

instance ToDoc Ly.BookPartElem where
  toDoc (Ly.BookPartScore score) =
      toDoc score

instance ToDoc Ly.Score where
  toDoc score = section "score" $ mconcat [
        toDoc score.header
      , toDoc score.elems
      ]

instance ToDoc Ly.ScoreElem where
  toDoc (Ly.ScoreStaff props content) = mconcat [
        section "layout" $ mconcat [
            section "context" $
              Doc.when props.hideTimeSignature $
                -- https://lilypond.org/doc/v2.24/Documentation/notation/visibility-of-objects
                Doc.line "\\Staff \\override TimeSignature.stencil = ##f"
          , section "context" $
              Doc.when props.omitMeasureNumbers $
                -- https://lilypond.org/doc/v2.24/Documentation/snippets/rhythms#rhythms-removing-bar-numbers-from-a-score
                Doc.line "\\Score \\omit BarNumber"
          ]
      , "<<"
      , Doc.indent $ mconcat [
            section "chords" $
              Doc.line $ renderChordNames content
          , section "new Staff" $
              Doc.line $ renderNotes content
          ]
      , ">>"
      ]
    where

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

instance ToDoc Ly.Header where
  toDoc header = section "header" $ mconcat [
        Doc.whenJust header.dedication  $ assign "dedication"
      , Doc.whenJust header.title       $ assign "title"
      , Doc.whenJust header.subtitle    $ assign "subtitle"
      , Doc.whenJust header.subsubtitle $ assign "subsubtitle"
      , Doc.whenJust header.instrument  $ assign "instrument"
      , Doc.whenJust header.poet        $ assign "poet"
      , Doc.whenJust header.composer    $ assign "composer"
      , Doc.whenJust header.meter       $ assign "meter"
      , Doc.whenJust header.arranger    $ assign "arranger"
      , Doc.whenJust header.tagline     $ assign "tagline"
      , Doc.whenJust header.copyright   $ assign "copyright"
      , Doc.whenJust header.piece       $ assign "piece"
      , Doc.whenJust header.opus        $ assign "opus"
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

section :: String -> Doc -> Doc
section name body = mconcat [
      Doc.line $ "\\" ++ name ++ "{"
    , Doc.indent body
    , "}"
    ]

assign :: String -> String -> Doc
assign var value = Doc.line $ var ++ " = \"" ++ value ++ "\""

assignReal :: String -> Double -> Doc
assignReal var value = Doc.line $ var ++ " = " ++ show value