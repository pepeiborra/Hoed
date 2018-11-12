{-# LANGUAGE DeriveGeneric #-}
module Debug.Hoed.Types where

import Control.Concurrent.MVar
import qualified Data.HashMap.Strict as H
import Data.Strict.Tuple (Pair(..))
import Data.Text
import Data.Word
import GHC.Generics

type UID = Int

data Event = Event { eventParent :: {-# UNPACK #-} !Parent
                   , change      ::                !Change  }
        deriving (Eq,Generic)

data EventWithId = EventWithId {eventUID :: !UID, event :: !Event}
  deriving (Eq)

data Change
        = Observe          !Text
        | Cons     !Word8  !Text
        | ConsChar         !Char
        | Enter
        | Fun
        deriving (Eq, Show,Generic)

type ParentPosition = Word8

data Parent = Parent
        { parentUID      :: !UID            -- my parents UID
        , parentPosition :: !ParentPosition -- my branch number (e.g. the field of a data constructor)
        } deriving (Eq,Generic)

instance Show Event where
  show e = (show . change $ e) ++ " (" ++ (show . eventParent $ e) ++ ")"

instance Show EventWithId where
  show (EventWithId uid e) = (show uid) ++ ": " ++ (show . change $ e) ++ " (" ++ (show . eventParent $ e) ++ ")"

instance Show Parent where
  show p = "P " ++ (show . parentUID $ p) ++ " " ++ (show . parentPosition $ p)

root = Parent (-1) 0

isRootEvent :: Event -> Bool
isRootEvent e = case change e of Observe{} -> True; _ -> False
