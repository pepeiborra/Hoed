{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Debug.Hoed.Streaming
  ( StreamingTrace
  , sendEvent
  , sendEventToHandle
  , endEventStream
  , endEventStreamHandle
  -- for testing
  , encode
  , decode
  , reorderBy
  , readChange
  , showsChange
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.ByteString.Char8             as BS
import           Data.Char                         (ord)
import           Data.Function                     (on)
import qualified Data.HashMap.Strict               as H
import           Data.IORef
import           Data.List
import           Data.Strict.Tuple                 (Pair (..))
import           Data.Text                         (pack, unpack)
import           Data.Text.Encoding
import           Data.Vector.Fusion.Bundle.Monadic (Bundle (..), cons,
                                                    fromStream)
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import           Data.Vector.Fusion.Bundle.Size
import           Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic               as V
import qualified Data.Vector.Generic.Mutable       as VM
import           Data.Vector.Unboxed               (Vector)
import           Debug.Hoed.Strings
import           Debug.Hoed.Types                  (Change (..), Event (..),
                                                    EventWithId (..),
                                                    Parent (..), StreamingTrace,
                                                    initEvent)
import           Std.IO.Buffered
import           Std.IO.FileSystem
import           Std.IO.Resource
import           System.Environment
import           System.FilePath
import           System.IO
import           System.IO.Unsafe
import Data.Bits
import Data.String

{-# NOINLINE sink #-}
sink :: (FilePath, BufferedOutput UVFileWriter, IO ())
sink = unsafePerformIO $ do
  p <- getExecutablePath
  let fp = p <> ".trace"
  -- h <- openBinaryFile fp WriteMode
  (h, rel) <- acquire $ initUVFile (fromString fp) (O_RDWR .|. O_CREAT) DEFAULT_MODE
  wri <- newUVFileWriter h 0
  buf <- newBufferedOutput wri 1024
  return (fp,buf,rel)

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent = sendEventToHandle (snd3 sink)

sendEventToHandle :: Output o => BufferedOutput o -> Int -> Parent -> Change -> IO ()
sendEventToHandle handle nodeId parent change = do
  atomicModifyIORef' counter (\x ->(succ x, ()))
  -- B.hPutStrLn handle $ "1 2 3 C 1 Foo" -- encode (EventWithId nodeId $ Event parent change) ""
  writeBuilder handle "1 2 3 C 1 Foo"
  return ()

{-# INLINE encode #-}
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

{-# INLINE showsChange #-}
showsChange :: Change -> ShowS
showsChange (Observe s)  = ("O " ++) . (show (unpack s) ++)
showsChange (Cons x t)   = ("C " ++) . shows x . (' ':) . (unpack t ++)
showsChange (ConsChar c) = ("D " ++) . shows (fromEnum c)
showsChange Enter        = ('E' :)
showsChange Fun          = ('F' :)

readChange :: String -> Change
readChange "E" = Enter
readChange "F" = Fun
readChange ('O' : ' ' : s) = Observe (pack (read s))
readChange ('D' : ' ' : c) = ConsChar (toEnum $ readInt c)
readChange ('C' : ' ' : rest) = Cons w (pack s)
  where
    (wstr, _:s) = span (/= ' ') rest
    w = fromIntegral $ readNat wstr
readChange other = error $ "Cannot read Change: " ++ other

readInt :: String -> Int
readInt ('-' : nat) = negate $ readNat nat
readInt nat         = readNat nat

readNat :: String -> Int
readNat = foldl' f 0
  where
    f d c = d * 10 + ord c - 48

-- | Ends the logging, sets up the string table, and returns the pure trace.
endEventStreamHandle :: Handle -> FilePath -> IO (StreamingTrace v)
endEventStreamHandle h fp = do
  hClose h
  -- Reopen the file in Read mode
  -- Read to a stream of events
  -- The stream can be processed in constant memory
  let trace  = readEvents $ openFile fp ReadMode
  -- For legacy reasons, the sequence starts with a dummy init event
  trace <- return $ S.cons (EventWithId 0 initEvent) trace
  eventCount <- readIORef counter
  putStrLn $ "endEventStreamHandle: " ++ show eventCount ++ " events"

  -- create string table for Unbox instances, in case they are used
  -- TODO remove this when no longer needed
  flip S.mapM_ trace $ \case
            EventWithId _ (Event _ (Observe  s)) -> void $ lookupOrAddString s
            EventWithId _ (Event _ (Cons c   s)) -> void $ lookupOrAddString s
            _ -> return ()
  (stringsCount :!: stringsHashTable) <- readMVar strings
  let unsortedStrings = H.toList stringsHashTable
  let stringsTable = V.unsafeAccum (const id) (V.replicate stringsCount (error "uninitialized")) [(i,s) | (s,i) <- unsortedStrings]
  writeIORef stringsLookupTable stringsTable

  return $ fmap event $ reorderBy eventUID 0 $ fromStream trace (Exact $ eventCount+1)

endEventStream :: IO (StreamingTrace v)
endEventStream = do
  let (fp, buf, rel) = sink
  -- res <- endEventStreamHandle buf fp
  rel
  return B.empty

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
            Yield x st ->
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

snd3 (_,x,_) = x
