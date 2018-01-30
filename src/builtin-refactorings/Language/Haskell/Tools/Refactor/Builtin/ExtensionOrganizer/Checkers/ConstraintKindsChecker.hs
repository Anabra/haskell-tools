module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ConstraintKindsChecker where

import Name as GHC (isTyVarName)
import Kind as GHC (returnsConstraintKind)

import Control.Reference ((^?), (^.), (&))

import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup


chkConstraintKindsDecl :: CheckNode Decl
chkConstraintKindsDecl = conditional chkConstraintKindsDecl' ConstraintKinds

chkConstraintKindsDecl' :: CheckNode Decl
chkConstraintKindsDecl' d@(TypeDecl dh rhs)
  -- Has any constraints of form (x t1 t2)
  | ctxts <- universeBi rhs :: [Context]
  , any hasTyVarHeadAsserts ctxts
  = addOccurence ConstraintKinds d
  -- Right-hand side has kind Constraint
  | otherwise = do
  let ty = typeOrKindFromId . declHeadQName $ dh
  if hasConstraintKind ty || returnsConstraintKind ty
     then addOccurence ConstraintKinds d
     else return d
chkConstraintKindsDecl' d = return d

hasTyVarHeadAsserts :: Context -> Bool
hasTyVarHeadAsserts = hasAnyTyVarHeads . (^. contextAssertion)

hasAnyTyVarHeads :: Assertion -> Bool
hasAnyTyVarHeads (ClassAssert n _)
  | Just n' <- semanticsName n = isTyVarName n'
  | otherwise               = False
hasAnyTyVarHeads ta@TupleAssert{}
  | Just assertions <- ta ^? innerAsserts & annListElems
  = any hasAnyTyVarHeads assertions
hasAnyTyVarHeads _ = False

declHeadQName :: DeclHead -> QualifiedName
declHeadQName (NameDeclHead n)       = n ^. simpleName
declHeadQName (ParenDeclHead dh)     = declHeadQName dh
declHeadQName (DeclHeadApp dh _)     = declHeadQName dh
declHeadQName (InfixDeclHead _ op _) = op ^. operatorName