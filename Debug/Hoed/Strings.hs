module Debug.Hoed.Strings where

import Control.Concurrent.MVar
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Strict.Tuple (Pair(..))
import Data.Text (Text)
import qualified Data.Vector as V
import System.IO.Unsafe

{-# NOINLINE strings #-}
strings :: MVar(Pair Int (HashMap Text Int))
strings = unsafePerformIO $
  newMVar (0 :!: mempty)

{-# NOINLINE stringsLookupTable #-}
stringsLookupTable :: IORef (V.Vector Text)
stringsLookupTable = unsafePerformIO $ newIORef  mempty

lookupString :: Int -> Text
lookupString id =
  unsafePerformIO $ do
    table <- readIORef stringsLookupTable
    return (table V.! id)

lookupOrAddString :: Text -> IO Int
lookupOrAddString s = do
  (stringsCount :!: stringsTable) <- readMVar strings
  case H.lookup s stringsTable of
    Just x  -> return x
    Nothing -> do
      (stringsCount :!: stringsTable) <- takeMVar strings
      let (count', table', res) =
            case H.lookup s stringsTable of
              Just x -> (stringsCount, stringsTable, x)
              Nothing ->
                (stringsCount+1, H.insert s stringsCount stringsTable, stringsCount)
      putMVar strings (count' :!: table')
      return res
