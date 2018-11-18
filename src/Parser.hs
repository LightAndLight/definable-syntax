{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language TypeApplications #-}
module Parser where

import Control.Applicative ((<|>), empty, some, many, optional)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Char (isLower, isLetter, isSpace, isAscii)
import Data.ListLike (ListLike)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Earley
  ( Grammar, Prod, Report, rule, listLike, satisfy, token, terminal, parser
  , fullParses
  )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Syntax (Part(..), Decl(..), Expr(..))

data Token
  = Ident String
  | Keyword String
  | Newline Char
  | Spaces String
  | TkSyntax
  | TkLeftBracket
  | TkRightBracket
  deriving (Eq, Show)

tokenize :: [String] -> String -> Maybe [Token]
tokenize ctx =
  either (const Nothing) Just .
  MP.parse @Void (many p) "test"
  where
    p =
      Newline <$> MP.char '\n' <|>
      TkSyntax <$ MP.string "syntax" <|>
      TkLeftBracket <$ MP.char '[' <|>
      TkRightBracket <$ MP.char ']' <|>
      Spaces <$> some (MP.oneOf " \t") <|>
      Ident <$> MP.try (some (MP.satisfy isLetter) >>= \s -> if s `elem` ctx then empty else pure s) <|>
      Keyword <$> some (MP.satisfy isLetter) <|>
      Keyword <$> some (MP.satisfy $ \t -> not (isLetter t) && notElem t "[] \t\n")

anyWord :: Prod r e Token String
anyWord =
  terminal $
  \t -> case t of
    Keyword s -> Just s
    Ident s -> Just s
    _ -> Nothing

word :: String -> Prod r e Token Token
word s =
  satisfy $
  \t -> case t of
    Keyword s' -> s == s'
    Ident s' -> s == s'
    _ -> False

ident :: Prod r e Token String
ident =
  terminal $
  \t -> case t of
    Ident s -> Just s
    _ -> Nothing

newline :: Prod r e Token Token
newline =
  satisfy $
  \t -> case t of
    Newline{} -> True
    _ -> False

keyword :: String -> Prod r e Token String
keyword s =
  terminal $
  \t -> case t of
    Keyword s' | s == s' -> Just s
    _ -> Nothing

space :: Prod r e Token Token
space =
  satisfy $
  \t -> case t of
    Spaces{} -> True
    _ -> False

partGrammar
  :: ([Part String], Expr)
  -> Prod r e Token Expr
  -> Prod r e Token Expr
partGrammar (ps, val) expr = subst <$> go ps
  where
    remove x [] = []
    remove x ((x', a) : rest)
      | x == x' = remove x rest
      | otherwise = (x', a) : remove x rest

    subst (ns, Var s) = fromMaybe (Var s) $ lookup s ns
    subst (ns, App a b) = App (subst (ns, a)) (subst (ns, b))
    subst (ns, Lam x e) = Lam x $ subst (remove x ns, e)

    go [] = pure ([], val)
    go (Word s' : ps') = word s' *> space *> go ps'
    go (Hole s' : ps') = (\e -> first ((s', e) :)) <$> expr <*> go ps'

exprG :: [([Part String], Expr)] -> Grammar r (Prod r e Token Expr)
exprG ctx = do
  var <- rule $ Var <$> ident <* optional space
  rec
    app <- rule $ App <$> e <*> atom
    lam <-
      rule $
      Lam <$ keyword "\\" <* optional space <*>
      ident <* keyword "." <* optional space <*>
      e
    atom <-
      rule $
      var <|>
      keyword "(" *> optional space *> e <* keyword ")" <* optional space
    p <- rule $ foldr (\p g -> partGrammar p atom <|> g) empty ctx
    e <- rule $ atom <|> app <|> lam <|> p
  pure e

declG :: [([Part String], Expr)] -> Grammar r (Prod r e Token Decl)
declG ctx = do
  expr <- exprG ctx
  binding <-
    rule $
    Binding <$>
    ident <* space <* keyword "=" <* space <*>
    expr
  part <-
    rule $
    Hole <$ token TkLeftBracket <* optional space <*>
    ident <* optional space <* token TkRightBracket <* optional space <|>
    Word <$> anyWord <* optional space
  syntax <-
    rule $
    Syntax <$ token TkSyntax <* space <*>
    some part <* keyword "=" <* space <*>
    expr
  rule $ (binding <|> syntax) <* optional newline

parseDecl
  :: [([Part String], Expr)]
  -> [Token]
  -> Either ([Decl], Report e [Token]) Decl
parseDecl ctx s =
  case fullParses (parser $ declG ctx) s of
    ([p], _) -> Right p
    e -> Left e

parseDecls
  :: String
  -> Either ([Decl], Report e [Token]) [Decl]
parseDecls = go [] . lines
  where
    go ctx [] = Right []
    go ctx (l:ls) =
      case tokenize reserved l of
        Nothing -> error "tokenize failed"
        Just tks ->
          case parseDecl ctx tks of
            Right a@(Syntax n e) -> (a :) <$> go ((n, e) : ctx) ls
            Right a -> (a :) <$> go ctx ls
            Left e -> Left e
      where
        reserved =
          foldMap (\(a, _) -> foldMap (\case; Word a -> [a]; _ -> []) a) ctx
