-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

-- module PrettyPrinting.Chick.Branch
--   (
--   )
-- where

-- import Control.Monad.Reader
-- import Data.Default
-- import Language (Language (Chick))
-- import PrettyPrinting.Chick.Term as PPChick
-- import PrettyPrinting.PrettyPrintable
-- import PrettyPrinting.PrettyPrintableUnannotated
-- import Term.Term

-- instance PrettyPrintableUnannotated 'Chick (Branch α Variable) where
--   prettyDocU b = do
--     precs <- ask
--     return $ PPChick.prettyBranchDocPrec precs b

-- instance PrettyPrintable 'Chick (Branch α Variable) where
--   prettyDoc t = runReader (prettyDocU @'Chick t) def
--   prettyStr = prettyStrU @'Chick
