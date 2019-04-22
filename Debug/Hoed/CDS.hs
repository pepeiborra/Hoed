{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.Hoed.CDS
  ( CDS(..)
  , CDSSet
  , CDSsansUID(..)
  , eventsToCDS
  , findFn
  , renderSet
  , renderSet'
  , grp
  , nil
  , sep
  , text
  )
where

import           Control.DeepSeq
import           Data.Char                (isAlpha)
import           Data.Coerce
import           Data.Hashable
import           Data.List                (sort, unfoldr)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle.Monadic as BM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import           Data.Word
import           Debug.Hoed.Observe
import           Debug.Hoed.Util
import           GHC.Exts(IsList(..))
import           GHC.Generics
import           Text.PrettyPrint.FPretty hiding (sep, (<$>), text)
import qualified Text.PrettyPrint.FPretty as FPretty
import           Text.Read ()


data CDS = CDSNamed      !Text !UID    !CDSSet
         | CDSCons       !UID  !Text   ![CDSSet]
         | CDSFun        !UID  !CDSSet !CDSSet
         | CDSEntered    !UID
         | CDSTerminated !UID
         | CDSChar       !Char   -- only used internally in eventsToCDS
         | CDSString     !String -- only used internally in eventsToCDS
        deriving (Show,Eq,Ord,Generic)

instance NFData CDS

normalizeCDS :: CDS -> CDS
normalizeCDS (CDSString s) = CDSCons 0 (pack $ show s) []
normalizeCDS (CDSChar   s) = CDSCons 0 (pack $ show s) []
normalizeCDS other = other
type CDSSet = [CDS]

-- Monomorphized [Parent] for compactness
data ParentList = ParentCons !Int !Word8 ParentList | ParentNil
instance IsList ParentList where
  type Item ParentList = Parent
  toList = unfoldr (\case ParentNil -> Nothing ; ParentCons pp pc t -> Just (Parent pp pc,t))
  fromList = foldr (\(Parent pp pc) t -> ParentCons pp pc t) ParentNil

eventsToCDS :: StreamingTrace v -> IO CDSSet
eventsToCDS pairsB = do
  l <- BM.length pairsB
  mid_arr <- VM.replicate l ParentNil
  pairs <- VU.unsafeFreeze =<< VM.munstream pairsB

  let mid_pairs = BM.map (\(node, Event (Parent pnode pport) _) ->
                               (pnode+1, Parent node pport))
                $ BM.filter (\(_,e) -> change e /= Enter)
                $ BM.indexed pairsB
  flip BM.mapM_ mid_pairs $ \(i, Parent pp pc) ->
    VM.modify mid_arr (ParentCons pp pc) i

  mid_arr <- V.unsafeFreeze mid_arr

  let getNode'' ::  Int -> Change -> CDS
      getNode'' node change =
        case change of
         Observe str         -> let chd = normalizeCDS <$> getChild node 0
                                in CDSNamed str (getId chd node) chd
         Enter               -> CDSEntered node
         Fun                 -> CDSFun node (normalizeCDS <$> getChild node 0)
                                            (normalizeCDS <$> getChild node 1)
         ConsChar char       -> CDSChar char
         Cons portc cons
                             -> simplifyCons node cons
                                  [ getChild node (fromIntegral n)
                                  | n <- [0::Int .. fromIntegral portc - 1]]

      getId []                 i  = i
      getId (CDSFun i _ _:_) _    = i
      getId (_:cs)             i  = getId cs i

      getChild :: Int -> Word8 -> CDSSet
      getChild pnode pport =
        [ getNode'' content (change (pairs VG.! content))
        | Parent content pport' <- toList $ mid_arr VG.! succ pnode
        , pport == pport'
        ]

  return $ getChild (-1) 0

simplifyCons :: UID -> Text -> [CDSSet] -> CDS
simplifyCons _ "throw" [[CDSCons _ "ErrorCall" set]]
  = CDSCons 0 "error" set
simplifyCons _ ":" [[CDSChar !ch], [CDSCons _ "[]" []]]
  = CDSString [ch]
simplifyCons _ ":" [[CDSChar !ch], [CDSString s]]
  = CDSString (ch:s)
simplifyCons uid con xx = CDSCons uid con (map (map normalizeCDS) xx)

render :: Int -> Bool -> CDS -> Doc
render prec par (CDSCons _ ":" [cds1,cds2]) =
        if par && not needParen
        then doc -- dont use paren (..) because we dont want a grp here!
        else paren needParen doc
   where
        doc = grp (renderSet' 5 False cds1 <> text " : ") <>
              renderSet' 4 True cds2
        needParen = prec > 4
render prec par (CDSCons _ "," cdss) | length cdss > 0 =
        nest 2 (text "(" <> foldl1 (\ a b -> a <> text ", " <> b)
                            (map renderSet cdss) <>
                text ")")
render prec _par (CDSCons _ name cdss)
  | not (T.null name)
  , (not . isAlpha . T.head) name && length cdss > 1 = -- render as infix
        paren (prec /= 0)
                  (grp
                    (renderSet' 10 False (head cdss)
                     <> sep <> text name
                     <> nest 2 (foldr (<>) nil
                                 [ if null cds then nil else sep <> renderSet' 10 False cds
                                 | cds <- tail cdss
                                 ]
                              )
                    )
                  )
  | otherwise = -- render as prefix
        paren (not (null cdss) && prec /= 0)
                 ( grp
                   (text name <> nest 2 (foldr (<>) nil
                                          [ sep <> renderSet' 10 False cds
                                          | cds <- cdss
                                          ]
                                       )
                   )
                 )

{- renderSet handles the various styles of CDSSet.
 -}

renderSet :: CDSSet -> Doc
renderSet = renderSet' 0 False

renderSet' :: Int -> Bool -> CDSSet -> Doc
renderSet' _ _      [] = text "_"
renderSet' prec par [cons@(CDSCons {})]    = render prec par cons
renderSet' prec par cdss                   =
        nest 0 (text "{ " <> foldl1 (\ a b -> a <> line <>
                                    text ", " <> b)
                                    (map renderFn pairs) <>
                line <> text "}")

   where
        findFn_noUIDs :: CDSSet -> [([CDSSet],CDSSet)]
        findFn_noUIDs c = map (\(a,r,_) -> (a,r)) (findFn c)
        pairs = nubSorted (sort (findFn_noUIDs cdss))

renderFn :: ([CDSSet],CDSSet) -> Doc
renderFn (args, res)
        = grp  (nest 3
                (text "\\ " <>
                 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b)
                       nil
                       args <> softline <>
                 text "-> " <> renderSet' 0 False res
                )
               )

-- | Reconstructs functional values from a CDSSet.
--   Returns a triple containing:
--    1. The arguments, if any, or an empty list for non function values
--    2. The result
--    3. The id of the CDSFun, if a functional value.
findFn :: CDSSet -> [([CDSSet],CDSSet, Maybe UID)]
findFn = foldr findFn' []

findFn' :: CDS -> [([CDSSet], CDSSet, Maybe UID)] -> [([CDSSet], CDSSet, Maybe UID)]
findFn' (CDSFun i arg res) rest =
    case findFn res of
       [(args',res',_)] -> (arg : args', res', Just i) : rest
       _                -> ([arg], res, Just i) : rest
findFn' other rest = ([],[other], Nothing) : rest


paren :: Bool -> Doc -> Doc
paren False doc = grp (nest 0 doc)
paren True  doc = grp (text "(" <> doc <> text ")")

nil :: Doc
nil = Text.PrettyPrint.FPretty.empty
grp :: Doc -> Doc
grp = Text.PrettyPrint.FPretty.group
sep :: Doc
sep = softline  -- A space, if the following still fits on the current line, otherwise newline.
sp :: Doc
sp = text " "   -- A space, always.

-- TODO fork FPretty to build on Text instead of Strings
text :: Text -> Doc
text = FPretty.text . unpack


-- %************************************************************************
-- %*                                                                   *
-- \subsection{Custom Eq and Ord instances for CDS that gloss over UIDs}
-- %*                                                                   *
-- %************************************************************************

newtype CDSsansUID = CDSsansUID CDS

instance Eq CDSsansUID where
  CDSsansUID(CDSNamed t _ xx) == CDSsansUID(CDSNamed t' _ yy) =
    t == t' && coerce xx == (coerce yy :: [CDSsansUID])
  CDSsansUID (CDSCons _ t xx) == CDSsansUID(CDSCons _ t' yy)  =
    t == t'  && coerce xx == (coerce yy :: [[CDSsansUID]])
  CDSsansUID (CDSFun _ res args) == CDSsansUID (CDSFun _ res' args') =
    (coerce res :: [CDSsansUID]) == coerce res' && coerce args == (coerce args' :: [CDSsansUID])
  CDSsansUID x == CDSsansUID y = x == y

instance Ord CDSsansUID where
  CDSsansUID (CDSNamed t _ xx) `compare` CDSsansUID (CDSNamed t' _ yy) =
    (t, coerce xx :: [CDSsansUID]) `compare` (t', coerce yy)
  CDSsansUID (CDSCons _ t xx) `compare` CDSsansUID (CDSCons _ t' yy) =
    (t, coerce xx :: [[CDSsansUID]]) `compare` (t', coerce yy)
  CDSsansUID (CDSFun _ args res) `compare` CDSsansUID (CDSFun _ args' res') =
    (coerce args :: [CDSsansUID], coerce res :: [CDSsansUID]) `compare` (coerce args', coerce res')
  CDSsansUID x `compare` CDSsansUID y = x `compare` y

instance Hashable CDSsansUID where
  s `hashWithSalt` CDSsansUID (CDSNamed t _ xx) = s `hashWithSalt` t `hashWithSalt` (coerce xx :: [CDSsansUID])
  s `hashWithSalt` CDSsansUID (CDSCons _  t xx) = s `hashWithSalt` t `hashWithSalt` (coerce xx :: [[CDSsansUID]])
  s `hashWithSalt` CDSsansUID (CDSFun _ args res) = s `hashWithSalt` (coerce args :: [CDSsansUID]) `hashWithSalt` (coerce res :: [CDSsansUID])
