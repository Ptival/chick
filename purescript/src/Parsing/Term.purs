module Parsing.Term where

import Prelude
import Data.Array as Array
import Data.List as List
import Term.Raw as Raw
import Control.Alternative ((<|>))
import Control.Error.Util (hush)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Parsing.ModularParser (ModularParser(AtomParser, TopSelfNextParser), Parser3, binOpL, binOpR, foldP, lexeme, rword, symbol)
import Term.Binder (Binder(..), ignoreSymbol)
import Term.Term (TermX(..), abstractAnonymous, abstractBinder, arrowSymbol, holeSymbol, lambdaBodySymbol, lambdaSymbol)
import Term.Variable (Variable(..))
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser, try)
import Text.Parsing.StringParser.String (alphaNum, anyLetter, eof)

termP :: Parser (Raw.Term Variable)
termP = foldP parser

parser :: Array (Array (ModularParser (Raw.Term Variable)))
parser =
  -- low precedence
  [ [TopSelfNextParser letP, TopSelfNextParser lamP]
  --, [binOpN annotSymbol (Annot unit)]
  , [TopSelfNextParser namedPiP, binOpR arrowSymbol (\ τ1 τ2 -> Pi unit τ1 (abstractAnonymous τ2))]
  , [binOpL "" (App unit)]
  , [AtomParser holeP, AtomParser typeP, AtomParser varP]
  ]
  -- high precedence

variableP :: Parser Variable
variableP = Variable <$> identifier

binderP :: Parser (Binder Variable)
binderP = Binder <$> ((Nothing <$ symbol ignoreSymbol) <|> (Just <$> variableP))

holeP :: Parser (Raw.Term Variable)
holeP = Hole unit <$ symbol holeSymbol

lamP :: Parser3 (Raw.Term Variable)
lamP _topP selfP _nextP =
  lams
  <$> (try (symbol lambdaSymbol) *> List.someRec binderP)
  <*> (symbol lambdaBodySymbol *> selfP)
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
    lams :: List.List (Binder Variable) -> Raw.Term Variable -> Raw.Term Variable
    lams List.Nil      t = t
    lams (b List.: bs) t = Lam unit (abstractBinder b (lams bs t))

letP :: Parser3 (Raw.Term Variable)
letP topP selfP _nextP = do
  try $ rword "let"
  b <- binderP
  symbol "="
  t1 <- topP
  rword "in"
  t2 <- selfP
  pure $ Let unit t1 (abstractBinder b t2)

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
  Tuple bs τ1 <- try $ do
    symbol "("
    bs <- List.someRec binderP
    symbol ":"
    τ1 <- topP
    symbol ")"
    symbol "→" -- I don't think we can commit prior to this, unfortunately
    pure $ Tuple bs τ1
  τ2 <- selfP
  pure $ pis bs τ1 τ2
  where
    pis :: List.List (Binder Variable) -> Raw.Term Variable -> Raw.Term Variable -> Raw.Term Variable
    pis List.Nil      _  τ2 = τ2
    pis (b List.: bs) τ1 τ2 = Pi unit τ1 (abstractBinder b $ pis bs τ1 τ2)

typeP :: Parser (Raw.Term Variable)
typeP = Type <$ rword "Type"

varP :: Parser (Raw.Term Variable)
varP = Var <$> variableP

-- Running parsers

langP :: Parser (Raw.Term Variable)
langP = termP <* eof

runParserTerm :: String -> Either ParseError (Raw.Term Variable)
runParserTerm = runParser langP

parseMaybeTerm :: String -> Maybe (Raw.Term Variable)
parseMaybeTerm = hush <<< runParserTerm

reservedWords :: List.List String -- list of reserved words
reservedWords = Array.toUnfoldable
  [ "let"
  , "in"
  , "λ"
  ]

identifier :: Parser String
identifier = (lexeme <<< try) (q >>= check)
  where
    p       = Array.(:) <$> anyLetter <*> Array.many alphaNum
    q       = fromCharArray <$> p
    check x =
      if x `List.elem` reservedWords
      then fail $ fold $ ["keyword ", show x, "cannot be an identifier"]
      else pure x
