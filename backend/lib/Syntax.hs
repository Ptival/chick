{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Syntax where

import Control.Applicative
import Control.Applicative.Free
import Data.Functor.Free
import Parsing (identifier, rword, symbol)
import Term.Binder
import Term.Term
import Term.Variable
import Text.Megaparsec
import Text.Megaparsec.String
import Text.PrettyPrint.Annotated.WL

data Syntax τ a where
  Capture :: Free (Syntax τ) a -> Syntax τ a
  Either :: [Free (Syntax τ) a] -> [Free (Syntax τ) b] -> Syntax τ (Either a b)
  Keyword :: String -> Syntax τ ()
  Next :: Syntax τ τ
  Raw :: Parser a -> Syntax τ a
  Self :: Syntax τ τ
  Top :: Syntax τ τ
  Try :: [Free (Syntax τ) a] -> Syntax τ a

-- THIS IS NOT A FUNCTOR

-- either' :: Ap (Syntax τ) a -> Ap (Syntax τ) b -> Ap (Syntax τ) (Either a b)
-- either' a b = liftAp $ Either a b
--
-- (<||>) :: Ap (Syntax τ) a -> Ap (Syntax τ) b -> Ap (Syntax τ) (Either a b)
-- (<||>) = either'
--
-- keyword :: String -> Ap (Syntax τ) ()
-- keyword s = liftAp $ Keyword s
--
-- raw :: Parser a -> Ap (Syntax τ) a
-- raw p = liftAp $ Raw p
--
-- self :: Ap (Syntax τ) τ
-- self = liftAp $ Self
--
-- top :: Ap (Syntax τ) τ
-- top = liftAp $ Top

data ParserLabels τ = ParserLabels
  { topP :: Parser τ,
    selfP :: Parser τ,
    nextP :: Parser τ
  }
  deriving (Functor)

newtype ModularParser τ a = ModularParser
  { unModularParser :: ParserLabels τ -> Parser a
  }
  deriving (Functor)

instance Applicative (ModularParser τ) where
  pure v = ModularParser $ \_ -> pure v
  ff <*> fv = ModularParser $ \labels ->
    (unModularParser ff labels) <*> (unModularParser fv labels)

data IntPair = IntPair Int Int

(>*<) :: Applicative f => f a -> f b -> f (a, b)
(>*<) fa fb = do
  a <- fa
  b <- fb
  return (a, b)

-- variableSyntax :: Ap (Syntax τ) Variable
-- variableSyntax = Variable <$> raw identifier

-- binderSyntax :: Ap (Syntax τ) (Binder Variable)
-- binderSyntax = Binder <$> either (const Nothing) Just <$> (raw (symbol "_") <||> variableSyntax)

variableSyntax :: Free (Syntax τ) Variable
variableSyntax = Variable <$> _

letSyntax :: [Syntax t (Binder Variable, t, t)]
letSyntax =
  [ Keyword "let",
    Capture binderSyntax,
    -- TODO: "="
    Capture Top,
    Keyword "in",
    Capture Self
  ]

-- letSyntax :: Ap (Syntax t) (Binder Variable, t, t)
-- letSyntax = do
--   _  <- keyword "let"
--   b  <- binderSyntax
--   -- TODO: "="
--   t1 <- top
--   _  <- keyword "in"
--   t2 <- self
--   return (b, t1, t2)

interpretSyntaxAsParser :: Syntax τ a -> ModularParser τ a
interpretSyntaxAsParser syn = ModularParser $ \labels@ParserLabels {..} -> case syn of
  Either a b -> (Left <$> runpack a labels) <|> (Right <$> runpack b labels)
  Keyword s -> rword s
  Next -> nextP
  Raw p -> p
  Self -> selfP
  Top -> topP
  Try p -> try (runpack p labels)
  where
    unpack p ls = unModularParser (interpretSyntaxAsParser p) ls
    runpack p ls = unModularParser (runSyntaxAsParser p) ls

runSyntaxAsParser :: Ap (Syntax τ) a -> ModularParser τ a
runSyntaxAsParser syn = runAp interpretSyntaxAsParser syn

letParser :: ModularParser τ (Binder Variable, τ, τ)
letParser = runSyntaxAsParser letSyntax

interpretSyntaxAsPrinter :: Syntax τ a -> a -> Doc ()
interpretSyntaxAsPrinter syn input = case syn of
  Either a b ->
    case input of
      Left l -> runSyntaxAsPrinter a l
      Right r -> _
  Keyword s -> _
  Next -> _
  Raw p -> _
  Self -> _
  Top -> _
  Try p -> _
  where
    unpack p ls = unModularParser (interpretSyntaxAsParser p) ls
    runpack p ls = unModularParser (runSyntaxAsParser p) ls

runSyntaxAsPrinter :: Ap (Syntax τ) a -> a -> Doc ()
runSyntaxAsPrinter syn input = _ $ runAp interpretSyntaxAsPrinter

{-
data SyntaxOps τ a where
  Commit  ::           SyntaxOps τ ()
  Keyword :: String -> SyntaxOps τ ()
  Self    ::           SyntaxOps τ τ
  Top     ::           SyntaxOps τ τ

commit :: ∀ τ r. Member (SyntaxOps τ) r => Eff r ()
commit = send $ Commit @τ

keyword :: ∀ τ r. Member (SyntaxOps τ) r => String -> Eff r ()
keyword s = send $ Keyword @τ s

self :: Member (SyntaxOps τ) r => Eff r τ
self = send $ Self

top :: Member (SyntaxOps τ) r => Eff r τ
top = send $ Top

syntaxLet :: ∀ τ r. Member (SyntaxOps τ) r => Eff r (τ, τ)
syntaxLet = do
  keyword @τ "let"
  commit  @τ
  e1 <- top
  keyword @τ "in"
  e2 <- self
  return (e1, e2)

interpretSyntaxOpsParser :: ∀ τ a.
  Eff (SyntaxOps τ ': '[]) a -> Parser3 a
interpretSyntaxOpsParser = \case
  Val x -> _ -- return x
  E u q -> case extract u of
    Commit -> _
    _ -> _
-}
