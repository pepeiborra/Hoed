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
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS -Wall #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014

module Debug.Hoed.Render
(CompStmt(..)
,StmtDetails(..)
,stmtRes
,renderCompStmts
,CDS
,eventsToCDS
,noNewlines
,sortOn
) where
import           Control.DeepSeq
import           Control.Monad.Trans.State.Strict
import           Data.Coerce
import           Data.Hashable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text, pack, unpack)
import           Debug.Hoed.CDS
import           Debug.Hoed.Compat
import           Debug.Hoed.Observe
import           Debug.Hoed.Util
import           Text.PrettyPrint.FPretty hiding (sep, (<$>), text)
import           Text.Read ()


------------------------------------------------------------------------
-- The CompStmt type

-- MF TODO: naming here is a bit of a mess. Needs refactoring.
-- Indentifier refers to an identifier users can explicitely give
-- to observe'. But UID is the unique number assigned to each event.
-- The field equIdentifier is not an Identifier, but the UID of the
-- event that starts the observation. And stmtUIDs is the list of
-- UIDs of all events that form the statement.

data CompStmt = CompStmt { stmtLabel      :: !Text
                         , stmtIdentifier :: !UID
                         , stmtDetails    :: !StmtDetails
                         }
                deriving (Generic)

instance NFData CompStmt

instance Eq CompStmt where c1 == c2 = stmtIdentifier c1 == stmtIdentifier c2
instance Ord CompStmt where
  compare c1 c2 = compare (stmtIdentifier c1) (stmtIdentifier c2)

instance Hashable CompStmt where
  hashWithSalt s cs = hashWithSalt s (stmtIdentifier cs)

data StmtDetails
  = StmtCon { stmtCon :: Hashed Text
           ,  stmtPretty :: Hashed Text}
  | StmtLam { stmtLamArgs :: [Hashed Text]
           ,  stmtLamRes :: Hashed Text
           ,  stmtPretty :: Hashed Text}
  deriving (Generic)

instance NFData StmtDetails

stmtRes :: CompStmt -> Text
stmtRes = unhashed . stmtPretty . stmtDetails

instance Show CompStmt where
  show = unpack . stmtRes
  showList eqs eq = unlines (map show eqs) ++ eq

noNewlines :: String -> String
noNewlines = noNewlines' False
noNewlines' :: Bool -> String -> String
noNewlines' _ [] = []
noNewlines' w (s:ss)
 | w       && (s == ' ' || s == '\n') =       noNewlines' True ss
 | not w && (s == ' ' || s == '\n') = ' ' : noNewlines' True ss
 | otherwise                          = s   : noNewlines' False ss

------------------------------------------------------------------------
-- Render equations from CDS set

renderCompStmts :: (?statementWidth::Int) => CDSSet -> [CompStmt]
renderCompStmts cdss = runMemoM $ concat <$> mapM renderCompStmt cdss

-- renderCompStmt: an observed function can be applied multiple times, each application
-- is rendered to a computation statement

renderCompStmt :: (?statementWidth::Int) => CDS -> MemoM [CompStmt]
renderCompStmt (CDSNamed name uid set) = do
        let output = cdssToOutput set
        concat <$> mapM (renderNamedTop name uid) output

renderCompStmt other = error $ show other

prettySet cds = prettySet_noid(coerce cds)

prettySet_noid :: (?statementWidth::Int) => [CDSsansUID] -> MemoM(Hashed Text)
prettySet_noid = MemoM . memo (prettyW . renderSet . coerce)

renderNamedTop :: (?statementWidth::Int) => Text -> UID -> Output -> MemoM [CompStmt]
renderNamedTop name observeUid (OutData cds) = mapM f pairs
  where
    f (args, res, Just i) =
      CompStmt name i <$>
      (StmtLam <$> mapM prettySet args <*>
       prettySet res <*>
       pure (prettyW $ renderNamedFn name (args, res)))
    f (_, cons, Nothing) =
      CompStmt name observeUid <$>
      (StmtCon <$> prettySet cons <*>
       pure (prettyW $ renderNamedCons name cons))
    pairs = (nubSorted . sortOn argAndRes) pairs'
    pairs' = findFn [cds]
    argAndRes (arg, res, _) = (arg, res)
renderNamedTop name _ other = error $ show other

-- %************************************************************************
-- %*                                                                   *
-- \subsection{The CDS converting functions}
-- %*                                                                   *
-- %************************************************************************


renderNamedCons :: Text -> CDSSet -> Doc
renderNamedCons name cons
  = text name <> nest 2
     ( sep <> grp (text "= " <> renderSet cons)
     )

renderNamedFn :: Text -> ([CDSSet],CDSSet) -> Doc
renderNamedFn name (args,res)
  = text name <> nest 2
     ( sep <> foldr (\ a b -> grp (renderSet' 10 False a) <> sep <> b) nil args
       <> sep <> grp ("= " <> align(renderSet res))
     )
data Output = OutLabel Text CDSSet [Output]
            | OutData  CDS
              deriving (Eq,Ord,Show)

cdssToOutput :: CDSSet -> [Output]
cdssToOutput =  map cdsToOutput

cdsToOutput :: CDS -> Output
cdsToOutput (CDSNamed name _ cdsset)
            = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@OutLabel {} <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@CDSCons {} = OutData cons
cdsToOutput    fn@CDSFun {} = OutData fn

prettyW :: (?statementWidth::Int) => Doc -> (Hashed Text)
prettyW doc = hashed $ pack $ pretty ?statementWidth doc

-- %************************************************************************
-- %*                                                                   *
-- \subsection{Memoization of pretty calls}
-- %*                                                                   *
-- %************************************************************************

newtype MemoM a = MemoM (State (Map [CDSsansUID] (Hashed Text)) a) deriving (Applicative, Functor, Monad)

runMemoM :: MemoM a -> a
runMemoM (MemoM comp) = evalState comp mempty

memo :: Ord a => (a->b) -> a -> State (Map a b) b
memo f a = do
  table <- get
  case Map.lookup a table of
    Just b -> return b
    Nothing -> do
      let b = f a
      modify (Map.insert a b)
      return b
