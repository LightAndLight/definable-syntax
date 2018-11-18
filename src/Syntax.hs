module Syntax where

data Part a
  = Word a
  | Hole a
  deriving (Eq, Show)

data Decl
  = Binding String Expr
  | Syntax String [Part String] Expr
  deriving (Eq, Show)

data Expr
  = Var String
  | App Expr Expr
  | Lam String Expr
  deriving (Eq, Show)
