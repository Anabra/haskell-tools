{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Language.Haskell.Tools.Refactor.Builtin.HelloRefactor where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import Control.Reference ((.-))
import Debug.Trace (trace)
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . helloRefactor)

helloRefactor :: RealSrcSpan -> LocalRefactoring
helloRefactor sp = return . (nodesContained sp .- showPatType)

helloExpr :: Expr -> Expr
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) e

showPatType :: Pattern -> Pattern
showPatType p@(LitPat lit) = trace ("\n" ++ prettyPrint p ++ " :: " ++ (showOutputable . semanticsLitType $ lit)) p
showPatType p = trace ("\n" ++ prettyPrint p ++ " :: " ++ (showOutputable . semanticsType $ p)) p
