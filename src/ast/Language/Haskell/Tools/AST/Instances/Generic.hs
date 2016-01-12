{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveGeneric #-}
module Language.Haskell.Tools.AST.Instances.Generic where

import GHC.Generics
import Language.Haskell.Tools.AST.Module
import Language.Haskell.Tools.AST.Decl
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
deriving instance (Generic a, Generic (e a)) => Generic (Ann e a)
deriving instance (Generic a, Generic (e a)) => Generic (AnnMaybe e a)
deriving instance (Generic a, Generic (e a)) => Generic (AnnList e a)

-- Modules
deriving instance Generic a => Generic (Module a)
deriving instance Generic a => Generic (ModuleHead a)
deriving instance Generic a => Generic (ExportSpecList a)
deriving instance Generic a => Generic (ExportSpec a)
deriving instance Generic a => Generic (IESpec a)
deriving instance Generic a => Generic (SubSpec a)
deriving instance Generic a => Generic (ModulePragma a)
deriving instance Generic a => Generic (ImportDecl a)
deriving instance Generic a => Generic (ImportSpec a)
deriving instance Generic a => Generic (ImportQualified a)
deriving instance Generic a => Generic (ImportSource a)
deriving instance Generic a => Generic (ImportSafe a)
deriving instance Generic a => Generic (TypeNamespace a)
deriving instance Generic a => Generic (ImportRenaming a)

-- Declarations
deriving instance Generic a => Generic (Decl a)
deriving instance Generic a => Generic (ClassBody a)
deriving instance Generic a => Generic (GadtDeclList a)
deriving instance Generic a => Generic (ClassElement a)
deriving instance Generic a => Generic (DeclHead a)
deriving instance Generic a => Generic (InstBody a)
deriving instance Generic a => Generic (InstBodyDecl a)
deriving instance Generic a => Generic (GadtDecl a)
deriving instance Generic a => Generic (GadtField a)
deriving instance Generic a => Generic (FunDeps a)
deriving instance Generic a => Generic (FunDep a)
deriving instance Generic a => Generic (ConDecl a)
deriving instance Generic a => Generic (FieldDecl a)
deriving instance Generic a => Generic (Deriving a)
deriving instance Generic a => Generic (InstanceRule a)
deriving instance Generic a => Generic (InstanceHead a)
deriving instance Generic a => Generic (TypeEqn a)
deriving instance Generic a => Generic (KindConstraint a)
deriving instance Generic a => Generic (TyVar a)
deriving instance Generic a => Generic (Type a)
deriving instance Generic a => Generic (Kind a)
deriving instance Generic a => Generic (Context a)
deriving instance Generic a => Generic (Assertion a)
deriving instance Generic a => Generic (Expr a)
deriving instance Generic a => Generic (Stmt a)
deriving instance Generic a => Generic (CompStmt a)
deriving instance Generic a => Generic (FunBind a)
deriving instance Generic a => Generic (Pattern a)
deriving instance Generic a => Generic (PatternField a)
deriving instance Generic a => Generic (Splice a)
deriving instance Generic a => Generic (QQString a)
deriving instance Generic a => Generic (Match a)
deriving instance Generic a => Generic (Alt a)
deriving instance Generic a => Generic (Binds a)
deriving instance Generic a => Generic (Rhs a)
deriving instance Generic a => Generic (GuardedRhs a)
deriving instance Generic a => Generic (FieldUpdate a)
deriving instance Generic a => Generic (Bracket a)
deriving instance Generic a => Generic (TopLevelPragma a)
deriving instance Generic a => Generic (Rule a)
deriving instance Generic a => Generic (Annotation a)
deriving instance Generic a => Generic (MinimalFormula a)
deriving instance Generic a => Generic (ExprPragma a)
deriving instance Generic a => Generic (SourceRange a)
deriving instance Generic a => Generic (Number a)

-- Literal
deriving instance Generic a => Generic (Literal a)
deriving instance Generic a => Generic (Promoted a)

-- Base
deriving instance Generic a => Generic (Name a)
deriving instance Generic a => Generic (SimpleName a)
deriving instance Generic a => Generic (StringNode a)
deriving instance Generic a => Generic (DataOrNewtypeKeyword a)
deriving instance Generic a => Generic (DoKind a)
deriving instance Generic a => Generic (TypeKeyword a)
deriving instance Generic a => Generic (OverlapPragma a)
deriving instance Generic a => Generic (CallConv a)
deriving instance Generic a => Generic (ArrowAppl a)
deriving instance Generic a => Generic (Safety a)
deriving instance Generic a => Generic (Assoc a)
deriving instance Generic a => Generic (Precedence a)
deriving instance Generic a => Generic (PhaseControl a)
deriving instance Generic a => Generic (PhaseNumber a)
deriving instance Generic a => Generic (PhaseInvert a)