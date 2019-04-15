{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TemplateHaskell #-}
module Debug.Hoed.Types where

import Control.Concurrent.MVar
import qualified Data.HashMap.Strict as H
import Data.Strict.Tuple (Pair(..))
import Data.Text
import Data.Vector.Fusion.Bundle.Monadic (Bundle)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Deriving
import Data.Word
import Debug.Hoed.Strings
import GHC.Generics
import System.IO.Unsafe

type UID = Int

type StreamingTrace v = Bundle IO v Event
type Trace = Vector Event

data Event = Event { eventParent :: {-# UNPACK #-} !Parent
                   , change      ::                !Change  }
        deriving (Eq,Generic)

initEvent = Event (Parent 0 0) (Observe "init")

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

-- Storable instances
-- ------------------
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
