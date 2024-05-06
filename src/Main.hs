module Main (main) where

import System.Environment
import FFmpeg
import Vlc

main :: IO ()
main = do
  args <- getArgs

  let fileName = head args
  printMetadata fileName
  playFile fileName
  


