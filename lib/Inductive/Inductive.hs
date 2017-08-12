{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UnicodeSyntax #-}

module Inductive.Inductive
  ( Constructor(..)
  , Inductive(..)
  , constructorCheckedType
  , constructorCheckedType'
  , constructorRawType
  , constructorRawType'
  , inductiveRawType
  , inductiveRawType'
  , inductiveType
  , inductiveType'
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Default

-- import           Inductive.Constructor
import           Precedence
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.TypeChecked as C
import           Term.Variable
import           Text.PrettyPrint.Annotated.WL
import           Text.Printf

data Inductive α ν =
  Inductive
  { inductiveName         :: ν
  , inductiveParameters   :: [(Binder ν, TypeX α ν)]
  , inductiveIndices      :: [(Binder ν, TypeX α ν)]
  , inductiveConstructors :: [Constructor α ν]
  }

deriving instance (Eq α, Eq ν) => Eq (Inductive α ν)
deriving instance (Show α, Show ν) => Show (Inductive α ν)

data Constructor α ν =
  Constructor
  { constructorInductive  :: Inductive α ν
  , constructorName       :: ν
  , constructorParameters :: [(Binder ν, TypeX α ν)]
  , constructorIndices    :: [TypeX α ν]
  }

deriving instance (Eq α, Eq ν) => Eq (Constructor α ν)
instance (Show α, Show ν) => Show (Constructor α ν) where
  show (Constructor _ n ps is) = printf "Constructor _ %s %s %s" (show n) (show ps) (show is)

constructorType' :: ∀ α.
  α ->
  Variable ->
  [(Binder Variable, TermX α Variable)] ->
  [(Binder Variable, TermX α Variable)] ->
  [TermX α Variable] ->
  TypeX α Variable
constructorType' α indName indParams consParams consIndices =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var Nothing indName)
        indParams)
    consIndices)
  consParams
  where
    onIndex :: TermX α Variable -> TermX α Variable -> TermX α Variable
    onIndex i t = App α t i --(Raw.raw t) (Raw.raw i)
    onParam :: (Binder Variable, TermX α Variable) -> TermX α Variable -> TermX α Variable
    onParam (b, p) t = Pi α p (abstractBinder b t)
    onIndParam :: (Binder Variable, TermX α Variable) -> TermX α Variable -> TermX α Variable
    onIndParam (Binder (Just v), _) t = App α t (Var Nothing v)
    onIndParam (Binder Nothing,  _) t = App α t (Hole α)

constructorRawType' ::
  Variable ->
  [(Binder Variable, TermX α Variable)] ->
  [(Binder Variable, TermX α Variable)] ->
  [TermX α Variable] ->
  Raw.Type Variable
constructorRawType' indName indParams consParams consIndices =
  constructorType' () indName indParams' consParams' consIndices'
  where
    indParams'   = map (over _2 Raw.raw) indParams
    consParams'  = map (over _2 Raw.raw) consParams
    consIndices' = map Raw.raw consIndices

constructorRawType :: Constructor Raw.Raw Variable -> Raw.Type Variable
constructorRawType (Constructor (Inductive indName indParams _ _) _ consParams consIndices) =
  constructorRawType' indName indParams consParams consIndices

-- | Constructs the type of the inductive type
inductiveRawType' ::
  [(Binder Variable, Raw.Type Variable)] ->
  [(Binder Variable, Raw.Type Variable)] ->
  Raw.Type Variable
inductiveRawType' indParams indIndices =
  foldr onIndexOrParam Type (indParams ++ indIndices)
  where
    -- onIndexOrParam :: (Binder Variable, Raw.Type Variable) -> Raw.Type Variable -> Raw.Type Variable
    onIndexOrParam (b, ip) t = Pi () ip (abstractBinder b t)

-- | Constructs the type of the inductive type
inductiveRawType :: Inductive Raw.Raw Variable -> Raw.Type Variable
inductiveRawType (Inductive _ ps is _) = inductiveRawType' ps is

constructorCheckedType' ::
  Variable ->
  [(Binder Variable, C.Type Variable)] ->
  [(Binder Variable, C.Type Variable)] ->
  [C.Type Variable] ->
  C.Type Variable
constructorCheckedType' = constructorType' (C.Checked Type)

constructorCheckedType :: Constructor (C.Checked Variable) Variable -> C.Type Variable
constructorCheckedType (Constructor (Inductive indName indParams _ _) _ consParams consIndices) =
  constructorCheckedType' indName indParams consParams consIndices

-- | Sometimes, we can't call `inductiveType` because the constructors are not checked yet.
-- | `inductiveType'` lets us call with just the minimal information.
inductiveType' ::
  [(Binder Variable, C.Type Variable)] ->
  [(Binder Variable, C.Type Variable)] ->
  C.Type Variable
inductiveType' ps is =
  foldr onIndexOrParam Type (ps ++ is)
  where
    -- onIndexOrParam :: C.Type Variable -> C.Type Variable -> C.Type Variable
    onIndexOrParam (b, ip) t = Pi (C.Checked Type) ip (abstractBinder b t)

inductiveType :: Inductive (C.Checked Variable) Variable -> C.Type Variable
inductiveType (Inductive _ ps is _) = inductiveType' ps is

arrows :: [Doc a] -> Doc a
arrows = encloseSep mempty mempty (text " →")

prettyBindingDocU ::
  MonadReader PrecedenceTable m =>
  (Binder Variable, TermX α Variable) -> m (Doc ())
prettyBindingDocU (Binder b, t) =
  case b of
    Nothing -> prettyDocU t
    Just v -> do
      tDoc <- prettyDocU t
      return $ parens . fillSep $ [ prettyDoc v, text ":", tDoc ]

instance PrettyPrintableUnannotated (Inductive α Variable) where
  prettyDocU (Inductive n ps is cs) = do
    psDoc <- mapM prettyBindingDocU ps
    csDoc <- mapM prettyDocU cs
    isDoc <- mapM prettyBindingDocU is
    return $ vsep $
      [ fillSep $
        [ text "inductive"
        , prettyDoc n
        ]
        ++
        (
          -- mempty creates an unwanted space, so have to use []
          if length ps == 0
          then []
          else [encloseSep mempty mempty mempty psDoc]
        )
        ++
        [ text ":"
        , arrows (isDoc ++ [text "Type"])
        , text "where"
        ]
      ] ++ (map (indent 2) csDoc)

instance PrettyPrintableUnannotated (Constructor α Variable) where
  prettyDocU (Constructor (Inductive indName indParams _ _) cName cParams cIndices) = do
    cDoc <- prettyDocU (constructorRawType' indName indParams cParams cIndices)
    return $ fillSep
      [ prettyDoc cName
      , text ":"
      , cDoc
      ]

instance PrettyPrintable (Constructor α Variable) where
  prettyDoc c = runReader (prettyDocU c) def

instance PrettyPrintable (Inductive α Variable) where
  prettyDoc i = runReader (prettyDocU i) def
