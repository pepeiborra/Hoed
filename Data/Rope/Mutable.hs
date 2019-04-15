{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Rope.Mutable
  ( Rope
  , Data.Rope.Mutable.new
  , new'
  , fromList
  , write
  , reset
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable as F
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as Map
import Data.Primitive.MutVar
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Bundle.Size as Size
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Generic.Mutable as M (MVector, read, new)

data RopeState m v a = RopeState
  { ropeLastIndex :: !Int
  , ropeElements :: IntMap (v (PrimState m) a)
  , spillOver :: v (PrimState m) a
  }

-- | A mutable bag-like collection with atomic O(1) inserts
data Rope m v a = Rope
  { ropeState   :: MutVar (PrimState m) (RopeState m v a)
  , ropeDim     :: !Int -- ^ Size of internal vectors
  }

defaultRopeDim = 10000

{-# INLINE write #-}
write :: forall v a m . (PrimMonad m, MVector v a) => Rope m v a -> Int -> a -> m ()
write Rope {..} ix a = join $ atomicModifyMutVar' ropeState updateState
  where
    (d, r) = divMod ix ropeDim
    updateState :: RopeState m v a -> (RopeState m v a, m ())
    updateState st@RopeState {..}
      | Just v <- Map.lookup d ropeElements =
        ( st {ropeLastIndex = max ropeLastIndex ix}
        , M.unsafeWrite v r a)
      | otherwise =
        ( st
          { ropeElements = Map.insert d spillOver ropeElements
          , ropeLastIndex = max ropeLastIndex ix
          }
        , do M.unsafeWrite spillOver r a
             v <- M.new ropeDim
             atomicModifyMutVar' ropeState $ \st' -> (st' {spillOver = v}, ()))

new :: forall v a m . (PrimMonad m, MVector v a) => m (Rope m v a)
new = new' defaultRopeDim

new' :: forall v a m . (PrimMonad m, MVector v a) => Int -> m (Rope m v a)
new' ropeDim = do
  spillOver  <- M.new ropeDim
  ropeState <- newMutVar (RopeState (-1) mempty spillOver)
  return Rope{..}

-- | Reset the rope to the empty state, returning the current contents
reset :: forall v v' a m .
  (VG.Vector v a, PrimMonad m) => Rope m (VG.Mutable v) a -> m (B.Bundle m v' a)
reset it@Rope {..} = do
  RopeState{..} <-
    atomicModifyMutVar' ropeState $ \old ->
                                      (RopeState (-1) mempty (spillOver old), old)
  let lv =
        [ case Map.lookup i ropeElements of
            Just x -> M.mstream x
            Nothing -> error $ "block missing: " ++ show i -- VG.new ropeDim
        | i <- [0 .. if Map.null ropeElements
                    then -1
                    else maximum (Map.keys ropeElements)]
        ]
  let joined
        | h:t <- lv
        , s <- S.slice 0 (ropeLastIndex + 1) $ S.concatMap id (S.fromList lv)
        = B.fromStream s (Size.Exact (ropeLastIndex+1))
        | otherwise = B.empty
  return joined

fromList :: forall v m a. (PrimMonad m, MVector v a) => Int -> [a] -> m(Rope m v a)
fromList dim xx = do
  rope <- new' dim
  forM_ (zip [0..] xx) $ \(i,x) -> write rope i x
  return rope
