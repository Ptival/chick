module CodeMirror.Component where

import Prelude
import CSS as CSS
import CodeMirror.Style as CMS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ports.CodeMirror as PCM
import Ports.CodeMirror.Configuration as CFG
import RxJS.Observable as RX
--import CodeMirror.Position (Position, Position'(..), Strictness(..), addPosition, initialPosition, isBefore, isWithinRange)
--import CodeMirror.TextMarker (TextMarker, TextMarkerId, textMarkerColor, textMarkerOptions)
--import Control.Apply (lift2)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
--import Control.Monad.Loops (whileM)
import Control.Monad.State.Class (class MonadState, gets, modify)
--import Data.Array (elem, fromFoldable, snoc)
--import Data.Foldable (find)
import Data.Lens (lens, over, view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse, traverse_)
--import Data.Tuple (snd)
--import Halogen.HTML.CSS (style)
import Halogen.Query (RefLabel(..))

-- | Configuration variables

-- | This turns an ObservableT you wish to listen to forever into a callback.
-- | Note that this explicitly forgets about the Subscription, so you can't stop it.
observableToCallback :: ∀ e t. RX.ObservableT (Eff e) t -> (t -> Eff e Unit) -> Eff e Unit
observableToCallback o k = void $ join $ RX.subscribeNext k o

type State =
  { code           :: String
  , codeMirror     :: Maybe PCM.CodeMirror
  }

type Input =
  { code :: String
  }

initialState :: Input -> State
initialState { code } =
  { code
  , codeMirror     : Nothing
  }

data Query a
  = Init         a
  | HandleChange PCM.CodeMirrorChange (H.SubscribeStatus -> a)

data Message
  = Updated String

flex :: CSS.StyleM Unit
flex = do
  -- CSS.alignContent  $ CSS.stretch
  -- CSS.alignItems    $ CSS.stretch
  -- CSS.display       $ CSS.flex
  pure unit

flexCol :: CSS.StyleM Unit
flexCol = do
  -- flex
  -- CSS.flexDirection $ CSS.column
  pure unit

flexRow :: CSS.StyleM Unit
flexRow = do
  -- flex
  -- CSS.flexDirection $ CSS.row
  pure unit

render :: State -> H.ComponentHTML Query
render { code } =
  HH.div [ CMS.componentStyle ] $
  [ HH.div [ CMS.editorContextStyle ] $
    [ HH.div [ CMS.editorContainerStyle ] $
      [ HH.div
        [ HP.ref (RefLabel "codemirror")
        , CMS.editorStyle
        ]
        [ -- intentionally left empty, will be filled by CodeMirror
        ]
      ]
    ]
  ]

type DSL = H.ComponentDSL State Query Message

type CodeMirrorEffects e =
  ( avar    :: AVAR
  , console :: CONSOLE
  | e)

getDoc :: ∀ e m. MonadAff (CodeMirrorEffects e) m => DSL m (Maybe PCM.Doc)
getDoc = gets _.codeMirror >>= traverse \ cm -> H.liftEff $ PCM.getDoc cm

eval ::
  ∀ e m.
  MonadAff (CodeMirrorEffects e) m =>
  Query ~> DSL m
eval = case _ of

  HandleChange change status -> do
    H.liftEff $ log $ "Change, removed: " <> change.changeObj.removed
    getDoc >>= traverse_ \ doc -> do
      code <- H.liftEff $ PCM.getValue doc Nothing
      H.modify (_ { code = code })
      H.raise $ Updated code
    pure $ status H.Listening

  Init next -> do
    { code } <- H.get
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \ element -> do
      cm <- H.liftEff do
        PCM.codeMirror element
          (CFG.def { autofocus      = Just true
                   , lineNumbers    = Just true
                   , mode           = Just "text/x-ocaml"
                   , value          = Just code
                   }
          )
      H.liftEff $ PCM.setSize cm "100%" "100%"
      H.modify (_ { codeMirror = Just cm })
      subscribeTo
        (PCM.onCodeMirrorChange cm)
        HandleChange
    pure next

  where

    subscribeTo ::
      ∀ o.
      ((o -> Eff (CodeMirrorEffects e) Unit) -> Eff (CodeMirrorEffects e) Unit) ->
      (o -> ((H.SubscribeStatus -> H.SubscribeStatus) -> Query H.SubscribeStatus)) -> DSL m Unit
    subscribeTo es h = H.subscribe $ H.eventSource es (Just <<< H.request <<< h)

codeMirrorComponent ::
  ∀ e m.
  MonadAff (CodeMirrorEffects e) m =>
  H.Component HH.HTML Query Input Message m
codeMirrorComponent = H.lifecycleComponent
  { initialState : initialState
  , render
  , eval
  , receiver     : const Nothing
  , initializer  : Just $ H.action Init
  , finalizer    : Nothing
  }
