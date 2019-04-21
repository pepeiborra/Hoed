{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-|
Module      : Debug.Hoed
Description : Lighweight algorithmic debugging based on observing intermediate values.
Copyright   : (c) 2000 Andy Gill, (c) 2010 University of Kansas, (c) 2013-2018 Maarten Faddegon
License     : BSD3
Maintainer  : hoed@maartenfaddegon.nl
Stability   : experimental
Portability : POSIX

Hoed is a tracer and debugger for the programming language Haskell.

To locate a defect with Hoed you annotate suspected functions and compile as usual. Then you run your program, information about the annotated functions is collected. Finally you connect to a debugging session using a console.

With Hoed you can list and search observed functions applied to argument values and the result values. 
Hoed also provides algorithmic debugging.
An algorithmic debugger finds defects in programs by systematic search. 
The programmer directs the search by answering a series of yes/no questions about
the correctness of specific function applications and their results.
Hoed also allows the use of (QuickCheck-style) properties to answer automatically
some of the questions arising during algorithmic debugging, and to replace others
by simpler questions.

= Example usage

Let us consider the following program, a defective implementation of a parity function with a test property.

> import Test.QuickCheck
>
> isOdd :: Int -> Bool
> isOdd n = isEven (plusOne n)
>
> isEven :: Int -> Bool
> isEven n = mod2 n == 0
>
> plusOne :: Int -> Int
> plusOne n = n + 1
>
> mod2 :: Int -> Int
> mod2 n = div n 2
>
> prop_isOdd :: Int -> Bool
> prop_isOdd x = isOdd (2*x+1)
>
> main :: IO ()
> main = printO (prop_isOdd 1)
>
> main :: IO ()
> main = quickcheck prop_isOdd

Using the property-based test tool QuickCheck we find the counter example `1` for our property.

> ./MyProgram
> *** Failed! Falsifiable (after 1 test): 1

Hoed can help us determine which function is defective. We annotate the functions `isOdd`, `isEven`, `plusOne` and `mod2` as follows:

> import Debug.Hoed
>
> isOdd :: Int -> Bool
> isOdd = observe "isOdd" isOdd'
> isOdd' n = isEven (plusOne n)
>
> isEven :: Int -> Bool
> isEven = observe "isEven" isEven'
> isEven' n = mod2 n == 0
>
> plusOne :: Int -> Int
> plusOne = observe "plusOne" plusOne'
> plusOne' n = n + 1
>
> mod2 :: Int -> Int
> mod2 = observe "mod2" mod2'
> mod2' n = div n 2
>
> prop_isOdd :: Int -> Bool
> prop_isOdd x = isOdd (2*x+1)
>
> main :: IO ()
> main = printO (prop_isOdd 1)

After running the program a computation tree is constructed and the algorithmic debugger is launched in the console.

@

False

=== program terminated ===
Please wait while the computation tree is constructed...

=== Statistics ===

28 events
4 computation statements
4 nodes + 1 virtual root node in the computation tree
4 edges in computation tree
computation tree has a branch factor of 1.3333333333333333 (i.e the average number of children of non-leaf nodes)

=== Debug Session ===

hdb> adb
======================================================================= [0-0/4]
isOdd 3  = False
? 
right  Judge computation statements right
        according to the intended behaviour/specification of the function.
wrong  Judge computation statements wrong
        according to the intended behaviour/specification of the function.
======================================================================= [0-0/4]
isOdd 3  = False
? wrong
======================================================================= [1-0/4]
isEven 4  = False
? wrong
======================================================================= [2-0/4]
mod2 4  = 2
? wrong
======================================================================= [3-0/4]
Fault located! In:
mod2 4  = 2
hdb>
@

Read more about Hoed on its project homepage <https://wiki.haskell.org/Hoed>.

Papers on the theory behind Hoed can be obtained via <http://maartenfaddegon.nl/#pub>.

I am keen to hear about your experience with Hoed: where did you find it useful and where would you like to see improvement? You can send me an e-mail at hoed@maartenfaddegon.nl, or use the github issue tracker <https://github.com/MaartenFaddegon/hoed/issues>.
-}

{-# LANGUAGE CPP             #-}

module Debug.Hoed
  ( -- * Basic annotations
    observe
  , HoedOptions(..)
  , defaultHoedOptions

  -- * Build your own debugger with Hoed
  , HoedAnalysis(..)
  , runO'
  , CompTree
  , Vertex(..)
  , CompStmt(..)
  , Verbosity(..)

   -- * The Observable class
  , Observable(..)
  , (<<)
  , thunk
  , send
  , observeOpaque
  , observeBase
  , constrainBase
  , debugO
  , CDS
  , Generic

  -- * API to test Hoed itself
  , logO
  , showGraph
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import           Debug.Hoed.CompTree
import           Debug.Hoed.Observe
import           Debug.Hoed.Render
import           Debug.Hoed.Util
import           Debug.Hoed.Types (Trace)

import           Data.Foldable (toList)
import           Data.IORef
import           Prelude                      hiding (Right)
import           System.Clock
import           System.Console.Terminal.Size
import           System.Directory             (createDirectoryIfMissing)
import           System.IO
import           System.IO.Unsafe

import           GHC.Generics

import           Data.Graph.Libgraph



-- %************************************************************************
-- %*                                                                   *
-- \subsection{External start functions}
-- %*                                                                   *
-- %************************************************************************

-- Run the observe ridden code.


runOnce :: IO ()
runOnce = do
  f <- readIORef firstRun
  if f
    then writeIORef firstRun False
    else error "It is best not to run Hoed more that once (maybe you want to restart GHCI?)"

firstRun :: IORef Bool
{-# NOINLINE firstRun #-}
firstRun = unsafePerformIO $ newIORef True


-- | run some code and return the Trace
debugO :: IO a -> IO (StreamingTrace v)
debugO program =
     do { runOnce
        ; initUniq
        ; let errorMsg e = "[Escaping Exception in Code : " ++ show e ++ "]"
        ; ourCatchAllIO (do { _ <- program ; return () })
                        (hPutStrLn stderr . errorMsg)
        ; res <- endEventStream
        ; initUniq
        ; return res
        }

-- | The main entry point; run some IO code, and debug inside it.
--   After the IO action is completed, an algorithmic debugging session is started at
--   @http://localhost:10000/@ to which you can connect with your webbrowser.
--
-- For example:
--
-- @
--   main = runO $ do print (triple 3)
--                    print (triple 2)
-- @


data HoedAnalysis = HoedAnalysis
  { hoedTrace       :: Trace
  , hoedCompTree    :: CompTree
  }

-- | Configuration options for running Hoed
data HoedOptions = HoedOptions
  { verbose     :: Verbosity
  , prettyWidth :: Int
  }

-- | The default is to run silent and pretty print with a width of 110 characters
defaultHoedOptions :: HoedOptions
defaultHoedOptions = HoedOptions Silent 110

--------------------------------------------

-- |Entry point giving you access to the internals of Hoed. Also see: runO.
runO' :: HoedOptions -> IO a -> IO HoedAnalysis
runO' HoedOptions{..} program = let ?statementWidth = prettyWidth in do
  hSetBuffering stderr NoBuffering
  createDirectoryIfMissing True ".Hoed/"
  tProgram <- stopWatch
  condPutStrLn verbose "=== program output ===\n"
  events <- debugO program
  programTime <- tProgram
  condPutStrLn verbose $ "\n=== program terminated (" ++ show programTime ++ ") ==="

#if defined(DEBUG)
  writeFile ".Hoed/Events"     (unlines . map show $ toList events)
#endif
  l <- B.length events
  condPutStrLn verbose "\n=== Statistics ===\n"
  condPutStrLn verbose $ show l ++ " events"
  condPutStrLn verbose"Please wait while the computation tree is constructed..."

  tTrace <- stopWatch
  ti  <- traceInfo verbose events
  traceTime <- tTrace
  condPutStrLn verbose $ " " ++ show traceTime
  eventsV <- VG.freeze =<< VM.munstream events
  cdss <- eventsToCDS events
  let eqs  = renderCompStmts cdss
      !ds  = force $ dependencies ti
      ct   = mkCompTree eqs ds

  condPutStr verbose "Calculating the nodes of the computation graph"
  tCds <- stopWatch
  forM_ (zip [0..] cdss) $ \(i,x) -> do
    evaluate (force x)
    when (isPowerOf 2 i) $ condPutStr verbose "."
  cdsTime <- tCds
  condPutStrLn verbose $ " " ++ show cdsTime

  condPutStr verbose "Rendering the nodes of the computation graph"
  tEqs <- stopWatch
  forM_ (zip [0..] eqs) $ \(i,x) -> do
    evaluate (case stmtDetails x of
                       StmtCon c _ -> seq c ()
                       StmtLam args res _ -> args `seq` res `seq` ())
    when (isPowerOf 2 i) $ condPutStr verbose "."
  eqsTime <- tEqs
  condPutStrLn verbose $ " " ++ show eqsTime

#if defined(DEBUG)
  -- writeFile ".Hoed/Cdss"       (unlines . map show $ cdss2)
  writeFile ".Hoed/Eqs"        (unlines . map show $ toList eqs)
  writeFile ".Hoed/Deps"       (unlines . map show $ toList ds)
#endif
#if defined(TRANSCRIPT)
  writeFile ".Hoed/Transcript" (getTranscript (toList events) ti)
#endif

  let n  = length eqs
      b  = fromIntegral (length . arcs $ ct ) / fromIntegral ((length . vertices $ ct) - (length . leafs $ ct))
  condPutStrLn verbose $ show n ++ " computation statements"
  condPutStrLn verbose $ show ((length . vertices $ ct) - 1) ++ " nodes + 1 virtual root node in the computation tree"
  condPutStrLn verbose $ show (length . arcs $ ct) ++ " edges in computation tree"
  condPutStrLn verbose $ "computation tree has a branch factor of " ++ show b ++ " (i.e the average number of children of non-leaf nodes)"

  let compTime = traceTime + cdsTime + eqsTime
  condPutStrLn verbose $ "\n=== Debug Session (" ++ show compTime ++ ") ===\n"
  return $ HoedAnalysis eventsV ct

isPowerOf n 0 = False
isPowerOf n k | n == k         = True
              | k `mod` n == 0 = isPowerOf n (k `div` n)
              | otherwise      = False

-- | Trace and write computation tree to file. Useful for regression testing.
logO :: FilePath -> IO a -> IO ()
logO filePath program = {- SCC "logO" -} do
  HoedAnalysis{..} <- runO' defaultHoedOptions{verbose=Verbose} program
  writeFile filePath (showGraph hoedCompTree)
  return ()
#if __GLASGOW_HASKELL__ >= 710
-- NOTE: This instance is orphaned here deliberately.
--       Moving it will break polymorphic observations in GHC 8.2x
-- | A catch-all instance for non observable types that produces the opaque observation @<?>@.
instance {-# OVERLAPPABLE #-} Observable a where
  observer = observeOpaque "<?>"
  constrain _ _ = error "constrained by untraced value"
#endif

showGraph g = showWith g showVertex showArc
  where
    showVertex RootVertex = ("\".\"", "shape=none")
    showVertex v          = ("\"" ++ (escape . showCompStmt) v ++ "\"", "")
    showArc _ = ""
    showCompStmt = show . vertexStmt
