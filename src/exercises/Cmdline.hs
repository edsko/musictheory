module Cmdline (
    Cmdline(..)
  , getCmdline
  ) where

import Options.Applicative

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline{
      outputDir :: FilePath
    }

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = execParser $ info (parseCmdline <**> helper) $ mconcat [
      header "Generate PDF (through Lilypond) with chord exercises"
    ]

{-------------------------------------------------------------------------------
  Parser proper
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    pure Cmdline
      <*> strOption (mconcat [
              short 'o'
            , help "Output directory"
            ])
