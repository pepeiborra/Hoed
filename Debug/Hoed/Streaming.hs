{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Debug.Hoed.Streaming
  ( StreamingTrace
  , Trace
  , sendEvent
  , sendEventToHandle
  , endEventStream
  , endEventStreamHandle
  -- for testing
  , endEventStreamB
  , encode
  , decode
  , reorderBy
  , readChange
  , showsChange
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Char (ord)
import Data.Function (on)
import Data.IORef
import Data.List
import Data.Text (pack, unpack)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Bundle.Monadic (Bundle(..), fromStream)
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import Data.Vector.Fusion.Bundle.Size
import Data.Vector.Unboxed (Vector)
import Debug.Hoed.Types(Change(..), Event(..), EventWithId(..), Parent(..))
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe

-- for compatibility
import Control.Concurrent.MVar
import Data.Strict.Tuple (Pair(..))
import qualified Data.HashMap.Strict as H
import Debug.Hoed.InProcess (strings, stringsLookupTable)

type Trace = Vector Event
type StreamingTrace v = Bundle IO v EventWithId

{-# NOINLINE sink #-}
sink :: (FilePath, Handle)
sink = unsafePerformIO $ do
  p <- getExecutablePath
  let fp = p ++ ".trace"
  h <- openFile fp WriteMode
  return (fp,h)

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent idx p c = do
  sendEventToHandle (snd sink) idx p c

sendEventToHandle :: Handle -> Int -> Parent -> Change -> IO ()
sendEventToHandle handle nodeId parent change = do
  _ <- atomicModifyIORef' counter (\x ->(succ x, succ x))
  hPutStrLn handle $ encode (EventWithId nodeId $ Event parent change) ""

encode :: EventWithId -> ShowS
encode (EventWithId i (Event (Parent puid ppos) change)) =
  shows i . (' ' :) . shows puid . (' ':) . shows ppos . (' ' :) . showsChange change

decode :: String -> EventWithId
decode s =
  case words s of
    (i:puid:ppos:c) ->
      EventWithId
        (readNat i)
        (Event
           (Parent (readInt puid) (fromIntegral $ readNat ppos))
           (readChange $ unwords c))
    other -> error $ "Unable to decode event: " ++ unwords other

showsChange :: Change -> ShowS
showsChange (Observe s) = ("O " ++) . (unpack s ++)
showsChange (Cons x t)  = ("C " ++) . shows x . (' ':) . (unpack t ++)
showsChange (ConsChar c) = ("D " ++) . shows (fromEnum c)
showsChange Enter = ('E' :)
showsChange Fun = ('F' :)

readChange :: String -> Change
readChange "E" = Enter
readChange "F" = Fun
readChange ('O' : ' ' : s) = Observe (pack s)
readChange ('D' : ' ' : c) = ConsChar (toEnum $ readInt c)
readChange ('C' : ' ' : rest) = Cons w (pack s)
  where
    (wstr, _:s) = span (/= ' ') rest
    w = fromIntegral $ readNat wstr
readChange other = error $ "Cannot read Change: " ++ other

readInt :: String -> Int
readInt ('-' : nat) = negate $ readNat nat
readInt nat = readNat nat

readNat :: String -> Int
readNat = foldl' f 0
  where
    f d c = d * 10 + ord c - 48

-- | Ends the logging and returns the raw complete trace
endEventStreamB :: Handle -> FilePath -> IO (StreamingTrace v)
endEventStreamB h fp = do
  hClose h
  -- Reopen the file in Read mode
  -- Read to a stream of events
  -- The stream can be processed in constant memory
  let s = readEvents $ openFile fp ReadMode
  u <- readIORef counter
  putStrLn $ "endEventStreamB: " ++ show u ++ " events"
  return $ fromStream s (Exact u)

-- | Ends the logging, sets up the string table, and returns the pure trace.
endEventStreamHandle ::
  V.Vector v Event => Handle -> FilePath -> IO (v Event)
endEventStreamHandle h fp = do
  b <- endEventStreamB h fp
  trace <- VM.munstream (fmap event (reorderBy eventUID 1 b))

  -- create string table for storable instances, in case they are used
  -- if they are not used, the table will be empty
  (stringsCount :!: stringsHashTable) <- takeMVar strings
  let unsortedStrings = H.toList stringsHashTable
  putMVar strings (0 :!: mempty)
  let stringsTable = V.unsafeAccum (\_ -> id) (V.replicate stringsCount (error "uninitialized")) [(i,s) | (s,i) <- unsortedStrings]
  writeIORef stringsLookupTable stringsTable

  V.unsafeFreeze trace

initEvent = Event (Parent 0 0) (Observe "init")

endEventStream :: IO Trace
endEventStream = do
  trace <- endEventStreamHandle (snd sink) (fst sink)
  -- For legacy reasons, the sequence starts with a dummy init event
  let trace' = V.cons initEvent trace
  return trace'

data ReorderState a
  = Buffer !Int
           [a]
  | Ordered !Int
  | Discharge !Int
              [a]
  | Stop [a]

-- | @reorderBy f i0 b@ produces a stream with the elements of @b@ in ascending order.
--   @i0@ must be the lowest index in the stream.
reorderBy :: Monad m => (a -> Int) -> Int -> Bundle m v a -> Bundle m v a
reorderBy ix i0 b@(sElems-> Stream step st) = fromStream s (sSize b)
  where
    s = flip Stream (Ordered i0, st) $ \(acc, st) ->
      case acc of
        Ordered i -> do
          stepst <- step st
          return $ case stepst of
            Skip st -> Skip (acc, st)
            Done -> Done
            Yield x st ->
              if ix x == i
                then Yield x (Ordered (succ i), st)
                else Skip (Buffer i [x], st)
        Buffer i buf -> do
          stepst <- step st
          return $ case stepst of
            Skip st -> Skip (acc,st)
            Yield x st -> do
              if ix x == i
                then Yield x (Discharge (i+1) buf, st)
                else Skip (Buffer i (insertBy (compare `on` ix) x buf), st)
            Done -> Skip (Stop buf, st)
        Discharge i [] -> return $ Skip (Ordered i, st)
        Discharge i (x:xx)
          | ix x == i -> return $ Yield x (Discharge (i+1) xx, st)
          | otherwise -> return $ Skip (Buffer i (x:xx), st)
        Stop [] -> return Done
        Stop (x:xx) -> return $ Yield x (Stop xx, st)

readEvents :: IO Handle -> Stream IO EventWithId
readEvents = fmap decode . streamLines

streamLines :: IO Handle -> Stream IO String
streamLines h = flip Stream h $ \h -> do
      h <- h
      done <- hIsEOF h
      if done
        then hClose h >> return Done
        else do
          l <- hGetLine h
          return $ Yield l (pure h)
