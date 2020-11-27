{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (..),
  )
where

import Prettyprinter
  ( Doc,
    defaultLayoutOptions,
    layoutPretty,
  )
import Prettyprinter.Render.String (renderString)
import Language (Language)

class PrettyPrintable (l :: Language) t where
  prettyDoc :: t -> Doc ()
  prettyStr :: t -> String
  prettyStr = renderString . layoutPretty defaultLayoutOptions . prettyDoc @l
  preview :: t -> String
  preview t =
    let s = prettyStr @l t
     in let previewLength = 20
         in if length s <= previewLength
              then s
              else (++ "...") . take 20 $ s

-- instance PrettyPrintable () where
--   prettyDoc () = text "()"

-- instance PrettyPrintable Bool where
--   prettyDoc False = text "False"
--   prettyDoc True  = text "True"

-- instance (PrettyPrintable l, PrettyPrintable r) => PrettyPrintable (l, r) where
--   prettyDoc (l, r) = encloseSep lparen rparen comma [prettyDoc l, prettyDoc r]

-- instance (PrettyPrintable a, PrettyPrintable b, PrettyPrintable c) =>
--          PrettyPrintable (a, b, c) where
--   prettyDoc (a, b, c) = encloseSep lparen rparen comma
--     [prettyDoc a, prettyDoc b, prettyDoc c]

-- instance (PrettyPrintable a) => PrettyPrintable [a] where
--   prettyDoc l = encloseSep lbracket rbracket comma (map prettyDoc l)

-- instance PrettyPrintable Int where
--   prettyDoc i = text (show i)
