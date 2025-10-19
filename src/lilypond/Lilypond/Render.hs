-- | Render to Lilypond file
--
-- Intended for qualified import
--
-- > import Lilypond.Render qualified as Ly
module Lilypond.Render (render) where

import Prelude hiding (elem)

import Data.Char (toLower)
import Data.List (intercalate)

import MusicTheory.Chord (Chord(Chord))
import MusicTheory.Chord qualified as Chord
import MusicTheory.Note (Note(Note))
import MusicTheory.Note qualified as Note

import Lilypond
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

instance ToDoc a => ToDoc [a] where
  toDoc = foldMap toDoc

{-------------------------------------------------------------------------------
  Render Lilypond fragments
-------------------------------------------------------------------------------}

instance ToDoc Lilypond where
  toDoc lilypond = mconcat [
        "\\version \"2.24.3\""
      , "\\language \"english\""
      , toDoc lilypond.header
      , toDoc lilypond.scores
      ]

instance ToDoc Header where
  toDoc header = section "header" $ mconcat [
        assign "title"    header.title
      , assign "composer" header.composer
      , assign "tagline"  ""
      ]

instance ToDoc Score where
  toDoc score = section "score" $ mconcat [
        toDoc score.header
      , toDoc score.elems
      ]

instance ToDoc ScoreHeader where
  toDoc header = section "header" $ mconcat [
        assign "piece" header.piece
      ]

instance ToDoc ScoreElem where
  toDoc (Staff content) = mconcat [
        "<<"
      , Doc.indent $ Doc.fromStrings [
            concat [
                "\\new ChordNames {"
              , renderChordNames content
              , "}"
              ]
          , concat [
                "\\new Staff {"
              , renderNotes content
              , "}"
              ]
          ]
      , ">>"
      ]
    where

{-------------------------------------------------------------------------------
  Auxiliary: render a single 'Staff'
-------------------------------------------------------------------------------}

renderChordNames :: Absolute -> String
renderChordNames (Absolute elems) = intercalate " " $ map aux elems
  where
    aux :: AbsoluteElem -> [Char]
    aux elem = concat [
          case elem.chordName of
            Nothing   -> error "TODO" -- should be a rest
            Just name -> renderChordName name
        , renderDuration elem.duration
        ]

renderNotes :: Absolute -> String
renderNotes (Absolute elems) = intercalate " " $
    map aux elems
  where
    aux :: AbsoluteElem -> String
    aux elem = concat [
          renderChord    elem.chord
        , renderDuration elem.duration
        ]

renderDuration :: Duration -> String
renderDuration Whole        = "1"
renderDuration Half         = "2"
renderDuration Quarter      = "4"
renderDuration Eighth       = "8"
renderDuration Sixteenth    = "16"
renderDuration Thirtysecond = "32"

{-------------------------------------------------------------------------------
  Chords
-------------------------------------------------------------------------------}

renderChordName :: ChordName -> String
renderChordName (ChordName note typ) = concat [
      renderSimpleNote note
    , renderChordType typ
    ]

-- <https://lilypond.org/doc/v2.24/Documentation/notation/common-chord-modifiers>
renderChordType :: Chord.Type -> String
renderChordType = \case
    Chord.TriadMajor -> ""
    Chord.TriadMinor -> ":m"

renderChord :: Chord -> String
renderChord (Chord ns) =
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
renderInOctave (Note.InOctave (Note n ma) o) = concat [
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
      Doc.fromString $ "\\" ++ name ++ "{"
    , Doc.indent body
    , "}"
    ]

assign :: String -> String -> Doc
assign var value = Doc.fromString $ var ++ " = \"" ++ value ++ "\""
