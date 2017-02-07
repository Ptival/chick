{-# language RankNTypes #-}

module ParseTerm where

import           Control.Applicative            (empty)
import           Data.Functor
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer          as L
import           Text.Megaparsec.String
import           Text.Printf

import           RawTerm
import           Term

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = empty -- L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

reservedWords :: [String] -- list of reserved words
reservedWords =
  [ "let"
  , "λ"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
      then fail $ printf "keyword %s cannot be an identifier" (show x)
      else return x

termP :: Parser RawTerm
termP = between sc eof termAndExpr

termAndExpr :: Parser RawTerm
termAndExpr = makeExprParser term expr

term :: Parser RawTerm
term =
  parens termAndExpr
  <|> holeP
  <|> letP
  <|> lamP -- :)
  <|> typeP
  <|> namedPiP
  <|> varP

-- _ @ _
-- _ → _

data Fixity
  = FixityL
  | FixityR
  | FixityN

data ExprEntry = MkExprEntry
  { operator ::
        Parser (RawTerm -> RawTerm -> RawTerm) -> Operator Parser RawTerm
  , factory :: RawTerm -> RawTerm -> RawTerm
  --, parser :: Parser a
  }

exprRaw :: [[(ExprEntry, Parser String)]]
exprRaw =
  -- high precedence
  [
    [ (MkExprEntry InfixL (App ()), symbol "") ]
  , [ (MkExprEntry InfixR (Pi () Nothing), symbol "→") ]
  , [ (MkExprEntry InfixN (Annot ()), symbol "@") ]
  ]
  -- low precedence

mkInfix :: Fixity -> m (a -> a -> a) -> Operator m a
mkInfix FixityL = InfixL
mkInfix FixityR = InfixR
mkInfix FixityN = InfixN

mkOperator :: (ExprEntry, Parser String) -> Operator Parser RawTerm
mkOperator (MkExprEntry fx fc, p) = fx (fc <$ p)

expr :: [[Operator Parser RawTerm]]
expr = (map . map) mkOperator exprRaw

{-
  -- high precedence
  [
    [ InfixL (App () <$ sc) ]
  , [ InfixR (Pi () Nothing <$ symbol "→") ]
  , [ InfixN (Annot () <$ symbol "@") ]
  ]
  -- low precedence
-}

annotP :: Parser RawTerm
annotP = do
  t <- term
  rword "@"
  τ <- term
  return $ Annot () t τ

appP :: Parser RawTerm
appP = do
  t1 <- term
  t2 <- term
  return $ App () t1 t2

holeP :: Parser RawTerm
holeP = do
  rword "_"
  return $ Hole ()

letP :: Parser RawTerm
letP = do
  rword "let"
  b <- binderP
  rword "="
  t1 <- term
  rword "in"
  t2 <- term
  return $ Let () b t1 t2

lamP :: Parser RawTerm
lamP = do
  rword "λ"
  b <- binderP
  rword "."
  t <- term
  return $ Lam () b t

anonymousPiP :: Parser RawTerm
anonymousPiP = do
  τ1 <- term
  rword "→"
  τ2 <- term
  return $ Pi () Nothing τ1 τ2

namedPiP :: Parser RawTerm
namedPiP = do
  rword "("
  b <- binderP
  rword ":"
  τ1 <- term
  rword ")"
  rword "→"
  τ2 <- term
  return $ Pi () b τ1 τ2

typeP :: Parser RawTerm
typeP = do
  rword "Type"
  return $ Type ()

varP :: Parser RawTerm
varP = do
  v <- identifier
  return $ Var () v

binderP :: Parser (Maybe Name)
binderP = (Nothing <$ rword "_") <|> (Just <$> identifier)
