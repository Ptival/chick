{-# language RankNTypes #-}

module Parsing where

import           Control.Applicative
import           Control.Monad.Fix
import           Data.Functor
--import           Data.Maybe
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String
import           Text.Printf

import           Parsing.Utils
import           Term.Binder
--import           Term.Bound
import           Term.Raw as Raw
import           Term.Term

type Parser1 a = Parser a -> Parser  a
type Parser2 a = Parser a -> Parser1 a
type Parser3 a = Parser a -> Parser2 a

data Associativity = LeftAssociative | RightAssociative | NonAssociative

data ModularParser a
  = TopSelfNextParser (Parser3 a)
  | BinaryOpParser Associativity String (a -> a -> a)
  | AtomParser (Parser a)

binOpL, binOpR, binOpN :: String -> (a -> a -> a) -> ModularParser a
binOpL = BinaryOpParser LeftAssociative
binOpR = BinaryOpParser RightAssociative
binOpN = BinaryOpParser NonAssociative

parser :: [[ModularParser (Raw.Term Variable)]]
parser =
  -- low precedence
  [ [TopSelfNextParser letP, TopSelfNextParser lamP]
  , [binOpN annotSymbol (Annot ())]
  , [TopSelfNextParser namedPiP, binOpR arrowSymbol (\ τ1 τ2 -> Pi () τ1 (abstractAnonymous τ2))]
  , [binOpL "" (App ())]
  , [AtomParser holeP, AtomParser typeP, AtomParser varP]
  ]
  -- high precedence

unModularP :: ModularParser a -> Parser3 a
unModularP (TopSelfNextParser p) = p
unModularP (BinaryOpParser LeftAssociative  s p) = binOpLP s p
unModularP (BinaryOpParser RightAssociative s p) = binOpRP s p
unModularP (BinaryOpParser NonAssociative   s p) = binOpNP s p
unModularP (AtomParser p) = \ _topP _selfP _nextP -> p

choiceOrNextP :: Parser a -> [ModularParser a] -> Parser1 a
choiceOrNextP topP ps nextP =
  fix $ \ selfP -> choice $ map (\ p -> unModularP p topP selfP nextP) ps ++ [nextP]

foldP :: [[ModularParser a]] -> Parser a
foldP ps = fix $ \ topP -> foldr ($) (parens topP) (map (choiceOrNextP topP) ps)

termP :: Parser (Raw.Term Variable)
termP = foldP parser

-- Binary operator parsers

binOpLP :: String -> (a -> a -> a) -> Parser3 a
binOpLP s k _topP _selfP nextP = chainl1 nextP (symbol s *> return k)

binOpRP :: String -> (a -> a -> a) -> Parser3 a
binOpRP s k _topP selfP nextP = k <$> try (nextP <* symbol s) <*> selfP

{-
-- short for:
binOpRP s k selfP nextP = do
  l <- try $ do
    l <- nextP
    symbol s
    return l
  r <- selfP
  return $ k l r
-}

binOpNP :: String -> (a -> a -> a) -> Parser3 a
binOpNP s k _selfP nextP = binOpRP s k nextP nextP

-- Individual parsers (alphabetically)

variableP :: Parser Variable
variableP = Variable <$> identifier

binderP :: Parser (Binder Variable)
binderP = Binder <$> ((Nothing <$ symbol wildcardSymbol) <|> (Just <$> variableP))

holeP :: Parser (Raw.Term Variable)
holeP = Hole () <$ symbol holeSymbol

lamP :: Parser3 (Raw.Term Variable)
lamP _topP selfP _nextP =
  lams
  <$> (try (symbol lamSymbol) *> some binderP)
  <*> (symbol postLamSymbol *> selfP)

{-
-- was:
  try $ do
    symbol "λ"
  bs <- some binderP
  symbol "."
  t <- selfP
  return $ lams bs t
-}
  where
    lams :: [Binder Variable] -> (Raw.Term Variable) -> (Raw.Term Variable)
    lams []       t = t
    lams (b : bs) t = Lam () (abstractBinder b (lams bs t))

letP :: Parser3 (Raw.Term Variable)
letP topP selfP _nextP = do
  try $ rword "let"
  b <- binderP
  symbol "="
  t1 <- topP
  rword "in"
  t2 <- selfP
  return $ Let () t1 (abstractBinder b t2)

{-
-- was:
  try $ do
    rword "let"
  b <- binderP
  symbol "="
  t1 <- termP
  rword "in"
  t2 <- selfP
  return $ Let () b t1 t2
-}

namedPiP :: Parser3 (Raw.Term Variable)
namedPiP topP selfP _nextP = do
  (bs, τ1) <- try $ do
    symbol forallSymbol
    symbol "("
    bs <- some binderP
    symbol ":"
    τ1 <- topP
    symbol ")"
    symbol "→" -- I don't think we can commit prior to this, unfortunately
    return (bs, τ1)
  τ2 <- selfP
  return $ pis bs τ1 τ2
  where
    pis :: [Binder Variable] -> (Raw.Term Variable) -> (Raw.Term Variable) -> (Raw.Term Variable)
    pis []       _  τ2 = τ2
    pis (b : bs) τ1 τ2 = Pi () τ1 (abstractBinder b $ pis bs τ1 τ2)

typeP :: Parser (Raw.Term Variable)
typeP = Type <$ rword "Type"

varP :: Parser (Raw.Term Variable)
varP = Var Nothing <$> variableP

-- Running parsers

langP :: Parser (Raw.Term Variable)
langP = termP <* eof

runParserTerm :: String -> Either (ParseError Char Dec) (Raw.Term Variable)
runParserTerm = runParser langP "runParserTerm"

parseMaybeTerm :: String -> Maybe (Raw.Term Variable)
parseMaybeTerm = parseMaybe langP

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
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if x `elem` reservedWords
      then fail $ printf "keyword %s cannot be an identifier" (show x)
      else return x
