module Main (main) where

import ClassReader
import Data.ByteString.Lazy qualified as BL

main :: IO ()
main = do
  classFile <- ClassReader.runClassReader <$> BL.readFile "./examples/SixSeven.class"
  case classFile of
    Left err -> print err
    Right classFile' -> print classFile'
