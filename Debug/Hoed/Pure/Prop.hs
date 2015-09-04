-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

module Debug.Hoed.Pure.Prop where
-- ( judge
-- , Property(..)
-- ) where

import Debug.Hoed.Pure.Observe(Trace(..),UID,Event(..),Change(..))
import Debug.Hoed.Pure.Render(CompStmt(..))
import Debug.Hoed.Pure.CompTree(CompTree,Vertex(..),Graph(..),vertexUID)
import Debug.Hoed.Pure.EventForest(EventForest,mkEventForest,dfsChildren)

import Prelude hiding (Right)
import Data.Graph.Libgraph(Judgement(..),mapGraph)
import System.Directory(createDirectoryIfMissing)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.IO(hPutStrLn,stderr)

------------------------------------------------------------------------------------------------------------------------

data Property =
  -- | Property which can be used to judge computation statements.
  Property { -- | Name of the (observed) function to which the property can be applied.
             funName :: String
             -- | Name of the module containing the property.
           , moduleName :: String
             -- | Name of the property.
           , propertyName :: String
             -- | Path to the source of the module containing the property. Can be a colon seperated list of directories.
           , searchPath :: String
           }


sourceFile = ".Hoed/exe/Main.hs"
buildFiles = ".Hoed/exe/Main.o .Hoed/exe/Main.hi"
exeFile    = ".Hoed/exe/Main"
outFile    = ".Hoed/exe/Main.out"


------------------------------------------------------------------------------------------------------------------------

lookupProperty :: [Property] -> Vertex -> Maybe Property
lookupProperty _ RootVertex = Nothing
lookupProperty properties v = lookupWith funName lbl properties
  where lbl = (stmtLabel . vertexStmt) v

lookupWith :: Eq a => (b->a) -> a -> [b] -> Maybe b
lookupWith f x ys = case filter (\y -> f y == x) ys of
  []    -> Nothing
  (y:_) -> Just y

------------------------------------------------------------------------------------------------------------------------

judge :: Trace -> [Property] -> CompTree -> IO CompTree
judge trc properties compTree = do
  ws <- mapM j vs
  return $ foldl updateTree compTree ws

  where vs = vertices compTree
        j v = case lookupProperty properties v of Nothing  -> return v
                                                  (Just p) -> judge1 trc p v
        updateTree compTree w = mapGraph (\v -> if (vertexUID v) == (vertexUID w) then w else v) compTree


-- Judge a vertex given a specific property
judge1 :: Trace -> Property -> Vertex -> IO Vertex
judge1 _ _ RootVertex = return RootVertex
judge1 trc prop v = do
  createDirectoryIfMissing True ".Hoed/exe"
  hPutStrLn stderr $ "Using property " ++ propertyName prop ++ " to judge statement " ++ (stmtRes . vertexStmt) v
  clean
  generateCode
  compile
  exit' <- compile
  hPutStrLn stderr $ "Exitted with " ++ show exit'
  exit  <- case exit' of (ExitFailure n) -> return (ExitFailure n)
                         ExitSuccess     -> evaluate
  out  <- readFile outFile
  hPutStrLn stderr $ "Exitted with " ++ show exit
  hPutStrLn stderr $ "Output is " ++ show out
  let jmt = judge1' exit out (vertexJmt v)
  hPutStrLn stderr $ "Judgement was " ++ (show . vertexJmt) v ++ ", and is now " ++ show jmt
  return v{vertexJmt= jmt}

  where clean        = system $ "rm -f " ++ sourceFile ++ " " ++ exeFile ++ " " ++ buildFiles
        generateCode = writeFile sourceFile (generate prop trc i)
        compile      = system $ "ghc  -i" ++ (searchPath prop) ++ " -o " ++ exeFile ++ " " ++ sourceFile
        evaluate     = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"
        i            = (stmtIdentifier . vertexStmt) v

-- The actual logic that changes the judgement of a vertex.
judge1' :: ExitCode -> String -> Judgement -> Judgement
judge1' (ExitFailure _) _   j = j
judge1' ExitSuccess     out j
  | out == "False\n" = Wrong
  | out == "True\n"  = j
  | otherwise     = j

------------------------------------------------------------------------------------------------------------------------

generate :: Property -> Trace -> UID -> String
generate prop trc i = generateHeading prop ++ generateMain prop trc i

generateHeading :: Property -> String
generateHeading prop =
  "-- This file is generated by the Haskell debugger Hoed\n"
  ++ "import " ++ moduleName prop ++ "\n"

generateMain :: Property -> Trace -> UID -> String
generateMain prop trc i =
  "main = print $ " ++ propertyName prop ++ " " ++ generateArgs trc i ++ "\n"

generateArgs :: Trace -> UID -> String
generateArgs trc i = case dfsChildren frt e of
  [_,ma,_,_]  -> generateExpr frt ma
  xs          -> error ("generateArgs: dfsChildren (" ++ show e ++ ") = " ++ show xs)

  where frt = (mkEventForest trc)
        e   = (reverse trc) !! (i-1)

generateExpr :: EventForest -> Maybe Event -> String
generateExpr _ Nothing    = __
generateExpr frt (Just e) = -- enable to add events as comments to generated code: "{- " ++ show e ++ " -}" ++
                            case change e of
  (Cons _ s) -> foldl (\acc c -> acc ++ " " ++ c) ("(" ++ s) cs ++ ") "
  Enter      -> ""
  _          -> "error \"cannot represent\""

  where cs = map (generateExpr frt) (dfsChildren frt e)

__ :: String
__ = "(error \"Request of value that was unevaluated in orignal program.\")"
