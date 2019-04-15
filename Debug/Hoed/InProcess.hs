{-# LANGUAGE BangPatterns #-}
module Debug.Hoed.InProcess where

import Control.Concurrent.MVar
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Rope.Mutable (Rope, new', write, reset)
import Data.Strict.Tuple (Pair(..))
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle.Monadic as BM
import Data.Vector.Unboxed.Mutable (MVector)
import Debug.Hoed.Streaming
import Debug.Hoed.Strings
import Debug.Hoed.Types
import System.Environment
import System.IO.Unsafe

-- Global store of unboxed events.
-- Since we cannot unbox Strings, these are represented as references to the
--  strings table
{-# NOINLINE events #-}
events :: Rope IO MVector Event
events = unsafePerformIO $ do
  rope <- new' 10000  -- size of the lazy vectors internal to the rope structure
  return rope

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent nodeId !parent !change = write events nodeId (Event parent change)

-- endEventStream :: IO (StreamingTrace v)
endEventStream :: IO (BM.Bundle IO v Event)
endEventStream = do
  res <- reset events
  (stringsCount :!: stringsHashTable) <- takeMVar strings
  let unsortedStrings = H.toList stringsHashTable
  putMVar strings (0 :!: mempty)
  let stringsTable =
        V.unsafeAccum
          (const id)
          (V.replicate stringsCount (error "uninitialized"))
          [(i, s) | (s, i) <- unsortedStrings]
  writeIORef stringsLookupTable stringsTable
  outputFile <- (++ ".vector") <$> getExecutablePath
  evs <- BM.toList res
  writeFile outputFile $ unlines $ zipWith (\i x -> encode (EventWithId i x) "") [0..] evs
  return res

