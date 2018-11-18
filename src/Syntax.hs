{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Syntax where

import Bound.Scope (Scope, abstract1, instantiate1)
import Bound.TH (makeBound)
import Data.Deriving (deriveEq1, deriveShow1)

lam :: Eq a => a -> Expr a -> Expr a
lam a e = Lam $ abstract1 a e

apply :: Expr a -> Expr a -> Expr a
apply a (Lam e) = instantiate1 a e
apply _ e = e

data Expr a
  = Var a
  | App (Expr a) (Expr a)
  | Lam (Scope () Expr a)
  deriving (Functor, Foldable, Traversable)
deriveEq1 ''Expr
deriveShow1 ''Expr
makeBound ''Expr
deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)

data Part a
  = Word a
  | Hole a
  deriving (Eq, Show)

data Decl
  = Binding String (Expr String)
  | Syntax [Part String] (Expr String)
  deriving (Eq, Show)
