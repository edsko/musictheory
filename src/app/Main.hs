module Main (main) where

import System.FilePath

import Lilypond.Render qualified as Ly

import Cmdline
import Exercises (exercises)

main :: IO ()
main = do
    cmdline <- getCmdline
    writeFile (cmdline.outputDir </> "music-theory-exercises.ly") $
      Ly.render exercises
