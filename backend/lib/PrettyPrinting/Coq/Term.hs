-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

-- module PrettyPrinting.Coq.Term where

-- import Control.Monad.Reader
-- import Data.Default
-- import Language (Language (Coq))
-- import Precedence
-- import PrettyPrinting.Chick.Term as PPChick
-- import PrettyPrinting.PrettyPrintable
-- import PrettyPrinting.PrettyPrintableUnannotated
-- import PrettyPrinting.Utils
-- import Term.Term

-- instance PrettyPrintableUnannotated Coq (Branch α Variable) where
--   prettyDocU b = do
--     precs <- ask
--     return $ PPChick.prettyBranchDocPrec precs b

-- instance PrettyPrintableUnannotated Coq (TermX α Variable) where
--   prettyDocU t = do
--     precs <- ask
--     return $ par precs (PrecMin, TolerateEqual) . PPChick.prettyTermDocPrec precs $ t

-- instance PrettyPrintable Coq (Branch α Variable) where
--   prettyDoc t = runReader (prettyDocU t) def
--   prettyStr = prettyStrU

-- instance PrettyPrintable Coq (TermX α Variable) where
--   prettyDoc t = runReader (prettyDocU t) def
--   prettyStr = prettyStrU
