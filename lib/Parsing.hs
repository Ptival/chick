{-# language RankNTypes #-}

module Parsing where

import           Control.Applicative
import           Data.Functor
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer          as L
import           Text.Megaparsec.String
import           Text.Printf

import           Term.RawTerm
import           Term

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = empty -- L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

reservedWords :: [String] -- list of reserved words
reservedWords =
  [ "let"
  , "in"
  , "λ"
  ]

identifierNoCheck :: Parser String
identifierNoCheck = (:) <$> letterChar <*> many alphaNumChar

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
      then fail $ printf "keyword %s cannot be an identifier" (show x)
      else return x

data ExprEntry = MkExprEntry
  { operator ::
        Parser (RawTerm -> RawTerm -> RawTerm) -> Operator Parser RawTerm
  , factory :: RawTerm -> RawTerm -> RawTerm
  , symbolString :: String
  }

-- exprRaw :: [[ExprEntry]]
-- exprRaw =
--   -- high precedence
--   [
--     [ MkExprEntry InfixL (App ())                 ""  ]
--   , [ MkExprEntry InfixR (Pi () (Binder Nothing)) "→" ]
--   , [ MkExprEntry InfixN (Annot ())               "@" ]
--   ]
--   -- low precedence

-- termP :: Parser RawTerm
-- termP = between sc eof termAndExpr

-- termAndExpr :: Parser RawTerm
-- termAndExpr = makeExprParser term expr

termP :: Parser RawTerm
termP = letlamP

piP :: Parser RawTerm
piP = choice [namedPiP, anonymousPiP, appP]
  where
    namedPiP = do
      (x, τ1) <- try $ do
        symbol "("
        x <- identifier
        symbol ":"
        τ1 <- termP
        symbol ")"
        symbol "→"
        return (x, τ1)
      τ2 <- piP
      return $ Pi () (Binder (Just x)) τ1 τ2
    anonymousPiP = do
      τ1 <- try $ do
        τ1 <- appP
        symbol "→"
        return τ1
      τ2 <- piP
      return $ Pi () (Binder Nothing) τ1 τ2

letlamP :: Parser RawTerm
letlamP = choice [letP, lamP, annotP]
  where

    letP = do
      try $ do
        rword "let"
      b <- binderP
      symbol "="
      t1 <- termP
      rword "in"
      t2 <- letlamP
      return $ Let () b t1 t2

    lamP = do
      try $ do
        symbol "λ"
      bs <- some binderP
      symbol "."
      t <- letlamP
      return $ lams bs t
      where
        lams :: [Binder] -> RawTerm -> RawTerm
        lams []       t = t
        lams (b : bs) t = Lam () b $ lams bs t

annotP :: Parser RawTerm
annotP = choice [goP, piP]
  where
    goP :: Parser RawTerm
    goP = do
      t <- try $ do
        t <- piP
        symbol "∷"
        return t
      τ <- piP
      return $ Annot () t τ

chainl1 :: (Alternative m, Monad m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do { f <- op ; y <- p ; rest (f x y) } <|> return x

appP :: Parser RawTerm
appP = choice [chainl1 restP $ space >> return (App ()), restP]

restP :: Parser RawTerm
restP = choice [atomP, parens termP]

atomP :: Parser RawTerm
atomP = choice [holeP, typeP, varP]

-- term :: Parser RawTerm
-- term =
--   try atomP

-- term :: Parser RawTerm
-- term =
--   try holeP
--   <|> try letP
--   <|> try lamP
--   <|> try typeP
--   <|> try namedPiP
--   <|> varP
--   <|> parens termAndExpr

-- mkOperator :: ExprEntry -> Operator Parser RawTerm
-- mkOperator (MkExprEntry fx fc p) = fx (fc <$ symbol p)

-- expr :: [[Operator Parser RawTerm]]
-- expr = (map . map) mkOperator exprRaw

{-
  -- high precedence
  [
    [ InfixL (App () <$ sc) ]
  , [ InfixR (Pi () Nothing <$ symbol "→") ]
  , [ InfixN (Annot () <$ symbol "@") ]
  ]
  -- low precedence
-}

-- annotP :: Parser RawTerm
-- annotP = do
--   t <- termAndExpr
--   _ <- symbol "@"
--   τ <- termAndExpr
--   return $ Annot () t τ

-- appP :: Parser RawTerm
-- appP = do
--   t1 <- termAndExpr
--   t2 <- termAndExpr
--   return $ App () t1 t2

holeP :: Parser RawTerm
holeP = do
  symbol "_"
  return $ Hole ()

typeP :: Parser RawTerm
typeP = do
  rword "Type"
  return $ Type ()

varP :: Parser RawTerm
varP = do
  v <- identifier
  return $ Var () v

binderP :: Parser Binder
binderP = Binder <$> ((Nothing <$ symbol "_") <|> (Just <$> identifier))

myRunParser :: Parser a -> String -> Either (ParseError (Token String) Dec) a
myRunParser p s = runParser (p <* eof) "ParseTerm" s
