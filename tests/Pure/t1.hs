{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.Vector.Generic as V
import Debug.Hoed
import System.Process(system)
import System.Exit(exitWith, ExitCode(..))

f :: Int -> Int
f = observe "f" f'
f' x = if x > 0 then g x else 0

g :: Int -> Int
g = observe "g" g'
g' x = x `div` 2

main = do
  HoedAnalysis{..} <- runO' defaultHoedOptions{verbose=Verbose} $ print ((f 2) + (f 0))
  writeFile  "hoed-tests-Pure-t1.graph" (showGraph hoedCompTree)
  i <- system "diff hoed-tests-Pure-t1.graph tests/ref/hoed-tests-Pure-t1.graph"
  unless (i == ExitSuccess) $ mapM_ print (V.toList hoedTrace)
  exitWith i
  

