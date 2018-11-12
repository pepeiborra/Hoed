{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Debug.Hoed.StreamingSpec (spec) where

import Control.Exception
import Data.Char
import Data.List
import qualified Data.Vector.Fusion.Bundle as B
import qualified Data.Vector.Fusion.Bundle.Monadic as BM
import qualified Data.Vector.Generic as V
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed
import Debug.Hoed.Streaming
import Debug.Hoed.Types
import System.Directory
import System.IO
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fun)

spec :: Spec
spec = do
  describe "reorder" $ do
    it "[]" $ B.toList (reorderBy id 0 B.empty) `shouldBe` []
    it "length (reorder x) == length x" $ do
      let x = B.fromList [2, 1, 3]
      B.length (reorderBy id 0 x) `shouldBe` B.length x
    prop "result is sorted" $ \(xx :: [Positive Int]) -> do
      B.toList (reorderBy getPositive 0 (B.fromList xx)) `shouldBe` sort xx
  describe "encoding" $ do
    describe "change" $ do
      it "readChange . showChange == id" $
        forAll genChange $ \c -> readChange (showsChange c "") == c
    describe "event" $ do
      it "decode . encode == id" $ forAll genEvent $ \x -> decode (encode x "") == x
  describe "sendEvent" $ do
    sendEventTest "no events" $ \h fp -> do
      evs <- endEventStreamB h fp
      BM.toList evs `shouldReturn` []
    sendEventTest "one event" $ \h fp -> do
      sendEventToHandle h 0 (Parent (-1) 0) (Observe "hello")
      evs <- endEventStreamB h fp
      BM.toList evs `shouldReturn`
        [EventWithId 0 $ Event (Parent (-1) 0) (Observe "hello")]
    sendEventTest "two events" $ \h fp -> do
      sendEventToHandle h 1 (Parent 0 0) Enter
      sendEventToHandle h 0 (Parent (-1) 0) (Observe "hello")
      evs <- endEventStreamB h fp
      BM.toList evs `shouldReturn`
        [EventWithId 1 $ Event (Parent 0 0) Enter
        ,EventWithId 0 $ Event (Parent (-1) 0) (Observe "hello")]

  describe "endEventStream(Boxed)" $ endEventsSpec @Boxed.Vector
  describe "endEventStream(Unboxed)" $ endEventsSpec @Unboxed.Vector

endEventsSpec :: forall (v :: * -> *) . V.Vector v Event => Spec
endEventsSpec = do
  sendEventTest "no events" $ \h fp -> do
    evs <- endEventStreamHandle @v h fp
    V.toList evs `shouldBe` []
  sendEventTest "one event" $ \h fp -> do
    sendEventToHandle h 0 (Parent (-1) 0) (Observe "hello")
    evs <- endEventStreamHandle @v h fp
    V.toList evs `shouldBe` [Event (Parent (-1) 0) (Observe "hello")]
  sendEventTest "two events" $ \h fp -> do
    sendEventToHandle h 1 (Parent 0 0) Enter
    sendEventToHandle h 0 (Parent (-1) 0) (Observe "hello")
    evs <- endEventStreamHandle @v h fp
    V.toList evs `shouldBe`
      [Event (Parent (-1) 0) (Observe "hello"), Event (Parent 0 0) Enter]

sendEventTest name k = it name $
  bracket
    (openTempFile "." "test")
    (\(fp, h) -> removeFile fp)
    (\(fp, h) -> k h fp)

genChange :: Gen Change
genChange =
  oneof
    [ pure Enter
    , pure Fun
    , pure $ Observe "Foo Bar"
    , Cons <$> arbitrary <*> pure "Foo Bar"
    , ConsChar <$> arbitrary
    ]

genEvent :: Gen EventWithId
genEvent =
  EventWithId <$> (getPositive <$> arbitrary) <*> (Event <$> genParent <*> genChange)

genParent :: Gen Parent
genParent = Parent <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)
