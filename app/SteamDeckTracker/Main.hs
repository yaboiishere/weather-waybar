module Main (main) where

import qualified Library
import RIO

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = Library.runMain
