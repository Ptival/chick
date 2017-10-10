{-# language RankNTypes #-}

module Parsing
  ( binderP
  , bindingP
  , bindingsP
  , parseMaybeTerm
  , runParserTerm
  , termP
  , variableP
  ) where

import           Control.Applicative
import           Control.Monad.Fix
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Parsing.Utils
import           Term.Binder
import           Term.Raw as Raw
import           Term.Term
import qualified Term.Universe as U

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
  [ [ TopSelfNextParser lamP
    , TopSelfNextParser letP
    , TopSelfNextParser matchP
    ]
  , [ binOpN annotSymbol (Annot ())]
  , [ TopSelfNextParser namedPiP
    , binOpR arrowSymbol (\ τ1 τ2 -> Pi () τ1 (abstractAnonymous τ2))
    ]
  , [ binOpL "" (App ()) ]
  , [ AtomParser holeP
    , AtomParser typeP, AtomParser varP
    ]
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
binOpRP s k _topP selfP nextP = try $ k <$> (nextP <* symbol s) <*> selfP
-- ^ we can't commit after the symbol, because it could be the prefix of
-- something else

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

matchP :: Parser3 (Raw.Term Variable)
matchP topP _selfP _nextP = do
  try $ rword "match"
  discriminee <- topP
  rword "with"
  branches <- many $ do
    symbol "|"
    ctor <- variableP
    args <- many binderP
    -- TODO: check for unicity of bound names
    symbol "=>"
    body <- topP
    return $ packBranch (ctor, args, body)
  rword "end"
  return $ Match () discriminee branches

namedPiP :: Parser3 (Raw.Term Variable)
namedPiP topP selfP _nextP = do
  groups <- try $ do
    symbol forallSymbol
    groups <- many $ do
      symbol "("
      bs <- some binderP
      symbol ":"
      τ1 <- topP
      symbol ")"
      return (bs, τ1)
    symbol postForallSymbol -- I don't think we can commit prior to this, unfortunately
    return groups
  τ2 <- selfP
  return $ pis groups τ2
  where
    pis :: [([Binder Variable], Raw.Term Variable)] -> (Raw.Term Variable) -> (Raw.Term Variable)
    pis [] τ2 = τ2
    pis (((b : bs), τ1) : τ1s) τ2 =
      Pi () τ1 (abstractBinder b $ pis ((bs, τ1) : τ1s) τ2)
    pis (([], _) : τ1s) τ2 = pis τ1s τ2

typeP :: Parser (Raw.Term Variable)
typeP = Type <$> (propP <|> setP <|> typeP')
  where
    propP  = U.Prop <$ rword "Prop"
    setP   = U.Set  <$ rword "Set"
    typeP' = U.Type <$ rword "Type"

varP :: Parser (Raw.Term Variable)
varP = Var Nothing <$> variableP

-- Running parsers

langP :: Parser (Raw.Term Variable)
langP = termP <* eof

runParserTerm :: String -> Either (ParseError Char Dec) (Raw.Term Variable)
runParserTerm = runParser langP "runParserTerm"

parseMaybeTerm :: String -> Maybe (Raw.Term Variable)
parseMaybeTerm = parseMaybe langP

bindingP :: Parser b -> Parser ([b], Term Variable)
bindingP bindP = parens $ do
  bs <- some bindP
  symbol ":"
  τ1 <- termP
  return (bs, τ1)

bindingsP :: Parser b -> Parser [([b], Term Variable)]
bindingsP bindP = many (bindingP bindP)
