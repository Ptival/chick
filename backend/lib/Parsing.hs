module Parsing
  ( binderP,
    bindingP,
    bindingsP,
    parseMaybeTerm,
    runParserTerm,
    termP,
    variableP,
  )
where

import Control.Monad.Fix (fix)
import Data.Functor (($>))
import Data.Void (Void)
import Parsing.Chick.Utils (identifier, parens, rword, symbol)
import Parsing.Types (Parser)
import Parsing.Utils (chainl1)
import Term.Binder (Binder (Binder))
import Term.Raw as Raw (Term)
import Term.Term
  ( GuardAndBody (GuardAndBody),
    TermX (Annot, App, Hole, Lam, Let, Match, Pi, Type, Var),
    Variable,
    abstractAnonymous,
    abstractBinder,
    annotSymbol,
    arrowSymbol,
    forallSymbol,
    hasTypeSymbol,
    holeSymbol,
    lamSymbol,
    mkVariable,
    packBranch,
    postForallSymbol,
    postLamSymbol,
    postLetSymbol,
    wildcardSymbol,
  )
import qualified Term.Universe as U
import qualified Text.Megaparsec as MP

type Parser1 a = Parser a -> Parser a

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
  [ [ TopSelfNextParser lamP,
      TopSelfNextParser letP,
      TopSelfNextParser matchP
    ],
    [binOpN annotSymbol (Annot ())],
    [ TopSelfNextParser namedPiP,
      binOpR arrowSymbol (\τ1 τ2 -> Pi () τ1 (abstractAnonymous τ2))
    ],
    [binOpL "" (App ())],
    [ AtomParser holeP,
      AtomParser typeP,
      AtomParser varP
    ]
  ]

-- high precedence

unModularP :: ModularParser a -> Parser3 a
unModularP (TopSelfNextParser p) = p
unModularP (BinaryOpParser LeftAssociative s p) = binOpLP s p
unModularP (BinaryOpParser RightAssociative s p) = binOpRP s p
unModularP (BinaryOpParser NonAssociative s p) = binOpNP s p
unModularP (AtomParser p) = \_topP _selfP _nextP -> p

choiceOrNextP :: Parser a -> [ModularParser a] -> Parser1 a
choiceOrNextP topP ps nextP =
  fix $ \selfP -> MP.choice $ map (\p -> unModularP p topP selfP nextP) ps ++ [nextP]

foldP :: [[ModularParser a]] -> Parser a
foldP ps = fix $ \topP -> foldr (choiceOrNextP topP) (parens topP) ps

termP :: Parser (Raw.Term Variable)
termP = foldP parser

-- Binary operator parsers

binOpLP :: String -> (a -> a -> a) -> Parser3 a
binOpLP s k _topP _selfP nextP = chainl1 nextP (symbol s $> k)

binOpRP :: String -> (a -> a -> a) -> Parser3 a
binOpRP s k _topP selfP nextP = MP.try $ k <$> (nextP <* symbol s) <*> selfP

-- unfortunately, we cannot commit after the symbol, because it could be the
-- prefix of something else

{-
-- short for:
binOpRP s k selfP nextP = try do
  l <- nextP
  symbol s
  r <- selfP
  return $ k l r
-}

binOpNP :: String -> (a -> a -> a) -> Parser3 a
binOpNP s k _selfP nextP = binOpRP s k nextP nextP

-- Individual parsers (alphabetically)

variableP :: Parser Variable
variableP = mkVariable <$> identifier

binderP :: Parser (Binder Variable)
binderP = Binder <$> ((Nothing <$ symbol wildcardSymbol) MP.<|> (Just <$> variableP))

holeP :: Parser (Raw.Term Variable)
holeP = Hole () <$ symbol holeSymbol

lamP :: Parser3 (Raw.Term Variable)
lamP _topP selfP _nextP =
  lams
    <$> (MP.try (symbol lamSymbol) *> MP.some binderP)
    <*> (symbol postLamSymbol *> selfP)
  where
    {-
    -- was:
      try $ do
        symbol "λ"
      bs <- some binderP
      symbol "."
      t <- selfP
      return $ lams bs t
    -}

    lams :: [Binder Variable] -> Raw.Term Variable -> Raw.Term Variable
    lams [] t = t
    lams (b : bs) t = Lam () (abstractBinder b (lams bs t))

letP :: Parser3 (Raw.Term Variable)
letP topP selfP _nextP = do
  MP.try $ rword "let"
  b <- binderP
  symbol postLetSymbol
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

guardP :: Parser (Maybe (Raw.Term Variable))
guardP = return Nothing -- FIXME: parse guards

matchP :: Parser3 (Raw.Term Variable)
matchP topP _selfP _nextP = do
  MP.try $ rword "match"
  discriminee <- topP
  rword "with"
  branches <- MP.many $ do
    symbol "|"
    ctor <- variableP
    args <- MP.many binderP
    -- TODO: check for unicity of bound names
    guard <- guardP
    symbol "=>"
    body <- topP
    return $ packBranch (ctor, args, GuardAndBody guard body)
  rword "end"
  return $ Match () discriminee branches

namedPiP :: Parser3 (Raw.Term Variable)
namedPiP topP selfP _nextP = do
  groups <- MP.try $ do
    symbol forallSymbol
    groups <- MP.many $ do
      symbol "("
      bs <- MP.some binderP
      symbol hasTypeSymbol
      τ1 <- topP
      symbol ")"
      return (bs, τ1)
    symbol postForallSymbol -- I don't think we can commit prior to this, unfortunately
    return groups
  τ2 <- selfP
  return $ pis groups τ2
  where
    pis :: [([Binder Variable], Raw.Term Variable)] -> Raw.Term Variable -> Raw.Term Variable
    pis [] τ2 = τ2
    pis (((b : bs), τ1) : τ1s) τ2 =
      Pi () τ1 (abstractBinder b $ pis ((bs, τ1) : τ1s) τ2)
    pis (([], _) : τ1s) τ2 = pis τ1s τ2

typeP :: Parser (Raw.Term Variable)
typeP = Type <$> (propP MP.<|> setP MP.<|> typeP')
  where
    propP = U.Prop <$ rword "Prop"
    setP = U.Set <$ rword "Set"
    typeP' = U.Type <$ rword "Type"

varP :: Parser (Raw.Term Variable)
varP = Var Nothing <$> variableP

-- Running parsers

langP :: Parser (Raw.Term Variable)
langP = termP <* MP.eof

runParserTerm :: String -> Either (MP.ParseErrorBundle String Void) (Raw.Term Variable)
runParserTerm = MP.runParser langP "runParserTerm"

parseMaybeTerm :: String -> Maybe (Raw.Term Variable)
parseMaybeTerm = MP.parseMaybe langP

bindingP :: Parser b -> Parser ([b], Term Variable)
bindingP bindP = parens $ do
  bs <- MP.some bindP
  symbol ":"
  τ1 <- termP
  return (bs, τ1)

bindingsP :: Parser b -> Parser [([b], Term Variable)]
bindingsP bindP = MP.many (bindingP bindP)
