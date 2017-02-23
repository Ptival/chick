{-# language RankNTypes #-}

module Parsing where

import           Control.Applicative
import           Control.Monad.Fix
import           Data.Functor
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String
import           Text.Printf

import           Term.RawTerm
import           Term

type SelfNextParser a = Parser a -> Parser a -> Parser a

parser :: [[SelfNextParser RawTerm]]
parser =
  -- low precedence
  [ [letP, lamP]
  , [annotP]
  , [namedPiP, anonymousPiP]
  , [appP]
  , [atomP]
  ]
  -- high precedence

choiceP :: [SelfNextParser a] -> Parser a -> Parser a
choiceP ps nextP =
  fix $ \ selfP -> choice $ map (\ p -> p selfP nextP) ps ++ [nextP]

foldP :: [[SelfNextParser a]] -> Parser a
foldP ps = fix $ \ self -> foldr ($) (parens self) (map choiceP ps)

termP :: Parser RawTerm
termP = foldP parser

-- Individual parsers

annotP :: SelfNextParser RawTerm
annotP _selfP nextP = do
  t <- try $ do
    t <- nextP
    symbol "∷"
    return t
  τ <- nextP
  return $ Annot () t τ

anonymousPiP :: SelfNextParser RawTerm
anonymousPiP selfP nextP = do
  τ1 <- try $ do
    τ1 <- nextP
    symbol "→"
    return τ1
  τ2 <- selfP
  return $ Pi () (Binder Nothing) τ1 τ2

appP :: SelfNextParser RawTerm
appP _selfP nextP = chainl1 nextP (space *> return (App ()))

atomP :: SelfNextParser RawTerm
atomP _selfP _nextP = choice [holeP, typeP, varP]

binderP :: Parser Binder
binderP = Binder <$> ((Nothing <$ symbol "_") <|> (Just <$> identifier))

holeP :: Parser RawTerm
holeP = Hole () <$ symbol "_"

lamP :: SelfNextParser RawTerm
lamP selfP _nextP = do
  try $ do
    symbol "λ"
  bs <- some binderP
  symbol "."
  t <- selfP
  return $ lams bs t
  where
    lams :: [Binder] -> RawTerm -> RawTerm
    lams []       t = t
    lams (b : bs) t = Lam () b $ lams bs t

letP :: SelfNextParser RawTerm
letP selfP _nextP = do
  try $ do
    rword "let"
  b <- binderP
  symbol "="
  t1 <- termP
  rword "in"
  t2 <- selfP
  return $ Let () b t1 t2

namedPiP :: SelfNextParser RawTerm
namedPiP selfP _nextP = do
  (x, τ1) <- try $ do
    symbol "("
    x <- identifier
    symbol ":"
    τ1 <- termP
    symbol ")"
    symbol "→"
    return (x, τ1)
  τ2 <- selfP
  return $ Pi () (Binder (Just x)) τ1 τ2

typeP :: Parser RawTerm
typeP = Type () <$ rword "Type"

varP :: Parser RawTerm
varP = Var () <$> identifier

-- Running parsers

runParserTerm :: String -> Either (ParseError Char Dec) RawTerm
runParserTerm = runParser (termP <* eof) "runParserTerm"

parseMaybeTerm :: String -> Maybe RawTerm
parseMaybeTerm = parseMaybe termP

-- Utilities

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

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

reservedWords :: [String] -- list of reserved words
reservedWords =
  [ "let"
  , "in"
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

chainl1 :: (Alternative m, Monad m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do { f <- op ; y <- p ; rest (f x y) } <|> return x
