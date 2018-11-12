{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies  #-}
module Debug.Hoed.InProcess where

import Control.Concurrent.MVar
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Proxy
import Data.Rope.Mutable (Rope, new', write, reset)
import Data.Strict.Tuple (Pair(..))
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Debug.Hoed.Types
import System.IO.Unsafe

type Trace = Vector Event

-- Global store of unboxed events.
-- Since we cannot unbox Strings, these are represented as references to the
--  strings table
{-# NOINLINE events #-}
events :: Rope IO MVector Event
events = unsafePerformIO $ do
  rope <- new' 10000  -- size of the lazy vectors internal to the rope structure
  return rope

{-# NOINLINE strings #-}
strings :: MVar(Pair Int (HashMap Text Int))
strings = unsafePerformIO $ do
  newMVar (0 :!: mempty)

{-# NOINLINE stringsLookupTable #-}
stringsLookupTable :: IORef (V.Vector Text)
stringsLookupTable = unsafePerformIO $ newIORef  mempty

lookupString id =
  unsafePerformIO $ do
    table <- readIORef stringsLookupTable
    when (id > V.length table) $
      fail $
      unwords
        [ "Internal error: strings table ("
        , show id
        , ", "
        , show (V.length table)
        , ")"
        ]
    return (V.unsafeIndex table id)

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent nodeId !parent !change = write events nodeId (Event parent change)

endEventStream :: IO Trace
endEventStream = do
  res <- reset (Proxy :: Proxy Vector) events
  (stringsCount :!: stringsHashTable) <- takeMVar strings
  let unsortedStrings = H.toList stringsHashTable
  putMVar strings (0 :!: mempty)
  let stringsTable =
        V.unsafeAccum
          (\_ -> id)
          (V.replicate stringsCount (error "uninitialized"))
          [(i, s) | (s, i) <- unsortedStrings]
  writeIORef stringsLookupTable stringsTable
  return res

-- Storable instances
-- ------------------

lookupOrAddString :: Text -> IO Int
lookupOrAddString s = do
  (stringsCount :!: stringsTable) <- readMVar strings
  case H.lookup s stringsTable of
    Just x  -> return x
    Nothing -> do
      (stringsCount :!: stringsTable) <- takeMVar strings
      let (count',table', res) =
            case H.lookup s stringsTable of
              Just x -> (stringsCount, stringsTable, x)
              Nothing ->
                (stringsCount+1, H.insert s stringsCount stringsTable, stringsCount)
      putMVar strings (count' :!: table')
      return res

derivingUnbox "Change"
    [t| Change -> (Word8, Word8, Int) |]
    [| \case
            Observe  s -> (0,0,unsafePerformIO(lookupOrAddString s))
            Cons c   s -> (1,c,unsafePerformIO(lookupOrAddString s))
            ConsChar c -> (2,0,fromEnum c)
            Enter      -> (3,0,0)
            Fun        -> (4,0,0)
     |]
    [| \case (0,_,s) -> Observe (lookupString s)
             (1,c,s) -> Cons c  (lookupString s)
             (2,_,c) -> ConsChar (toEnum c)
             (3,_,_) -> Enter
             (4,_,_) -> Fun
     |]

derivingUnbox "Parent"
    [t| Parent -> (UID, ParentPosition) |]
    [| \ (Parent a b) -> (a,b) |]
    [| \ (a,b) -> Parent a b |]

derivingUnbox "Event"
    [t| Event -> (Parent, Change) |]
    [| \(Event a b) -> (a,b) |]
    [| \ (a,b) -> Event a b |]
