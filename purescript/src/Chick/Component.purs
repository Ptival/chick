module Chick.Component where

import Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import PrettyPrint.PrettyPrintable1 (prettyStr1)
import StandardLibrary.Prelude (tFlip, tId, τFlip, τId)

data Query a
  = Init a

type State =
  {
  }

type Input = Unit

type Message = Void

type ChickEffects e =
  ( console :: CONSOLE
  | e)

type Render = H.ComponentHTML Query

render :: State -> Render
render state =
  HH.div
  [ style $ do
       CSS.fontFaceFamily $ "monospace"
  ]
  [ HH.text $ show $ prettyStr1 <$> τFlip
  -- [ HH.text $ show $ prettyStr1 <$> τId
  -- , HH.text $ show $ prettyStr1 <$> tId
  -- , HH.text $ show $ prettyStr1 <$> τFlip
  -- , HH.text $ show $ prettyStr1 <$> tFlip
  ]

type DSL = H.ComponentDSL State Query Message

eval :: ∀ e m. MonadAff (ChickEffects e) m => Query ~> DSL m
eval = case _ of

  Init next -> do
    pure next

chickComponent :: ∀ e m. MonadAff (ChickEffects e) m => H.Component HH.HTML Query Input Message m
chickComponent =
  H.component
    { initialState : const { }
    , render
    , eval
    , receiver     : const Nothing
    }
