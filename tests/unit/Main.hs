module Main where

import Test.Hspec
import qualified Data.Rope.Mutable.Spec as Rope
import qualified Debug.Hoed.StreamingSpec as Streaming

main :: IO ()
main = hspec spec

spec = do
  Rope.spec
  Streaming.spec
