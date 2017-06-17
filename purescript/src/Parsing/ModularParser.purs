module Parsing.ModularParser where

import Prelude
import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (foldr, singleton)
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.Combinators (chainl1, choice, many1, manyTill)
import Text.Parsing.StringParser.String (alphaNum, anyChar, satisfy, string)

type Parser1 a = Parser a -> Parser  a
type Parser2 a = Parser a -> Parser1 a
type Parser3 a = Parser a -> Parser2 a

data Associativity = LeftAssociative | RightAssociative | NonAssociative

data ModularParser a
  = TopSelfNextParser (Parser3 a)
  | BinaryOpParser Associativity String (a -> a -> a)
  | AtomParser (Parser a)

binOpL :: ∀ a. String -> (a -> a -> a) -> ModularParser a
binOpL = BinaryOpParser LeftAssociative

binOpR :: ∀ a. String -> (a -> a -> a) -> ModularParser a
binOpR = BinaryOpParser RightAssociative

binOpN :: ∀ a. String -> (a -> a -> a) -> ModularParser a
binOpN = BinaryOpParser NonAssociative

unModularP :: ∀ a. ModularParser a -> Parser3 a
unModularP (TopSelfNextParser p) = p
unModularP (BinaryOpParser LeftAssociative  s p) = binOpLP s p
unModularP (BinaryOpParser RightAssociative s p) = binOpRP s p
unModularP (BinaryOpParser NonAssociative   s p) = binOpNP s p
unModularP (AtomParser p) = \ _topP _selfP _nextP -> p

choiceOrNextP :: ∀ a. Parser a -> Array (ModularParser a) -> Parser1 a
choiceOrNextP topP ps nextP =
  fix $ \ selfP -> choice $ map (\ p -> unModularP p topP selfP nextP) ps <> singleton nextP

foldP :: ∀ a. Array (Array (ModularParser a)) -> Parser a
foldP ps = fix $ \ self -> foldr ($) (parens self) (map (choiceOrNextP self) ps)

binOpLP :: ∀ a. String -> (a -> a -> a) -> Parser3 a
binOpLP s k _topP _selfP nextP = chainl1 nextP (symbol s *> pure k)

binOpRP :: ∀ a. String -> (a -> a -> a) -> Parser3 a
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

comment :: Parser Unit
comment = do
  _ <- string "(*"
  _ <- manyTill anyChar $ string "*)"
  pure unit

skipSpace :: Parser Unit
skipSpace = fix \_ ->
  (comment *> skipSpace)
  <|> (many1 ws *> skipSpace)
  <|> pure unit
  where
    ws = satisfy \c ->
      c == '\n' ||
      c == '\r' ||
      c == '\t' ||
      c == ' '

lexeme :: ∀ a. Parser a -> Parser a
lexeme p = p <* skipSpace

binOpNP :: ∀ a. String -> (a -> a -> a) -> Parser3 a
binOpNP s k _selfP nextP = binOpRP s k nextP nextP

symbol :: String -> Parser Unit
symbol = void <<< lexeme <<< string

betweenP :: ∀ a. Parser Unit -> Parser Unit -> Parser a -> Parser a
betweenP open close p = do
  open
  x <- p
  close
  pure x

parens :: ∀ a. Parser a -> Parser a
parens = betweenP (symbol "(") (symbol ")")

notFollowedBy :: ∀ a. Show a => Parser a -> Parser Unit
notFollowedBy p =  try (go <|> pure unit)
  where
    go = do
      c <- try p
      fail (show c)

rword :: String -> Parser Unit
rword w = string w *> notFollowedBy alphaNum *> skipSpace
