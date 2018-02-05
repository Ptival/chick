module Chick.Component where

import CSS as CSS
import CodeMirror.Component as CM
import CodeMirror.Style (flexRow)
import Control.Applicative (pure)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, fromObject, fromString, stringify)
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Foreign.Generic (decodeJSON)
import Data.Function (const, ($))
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', lens, over)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (type (~>), bind, discard)

-- import Examples.ListToVec as EX
import Examples.SoftwareFoundations as EX

-- this seems a little dumb, but looks like we have to...
data Query a
  = AfterUpdated  String a
  | BeforeUpdated String a

type ChildQuery = Coproduct3 CM.Query CM.Query CM.Query

type State =
  { codeBefore :: String
  , codeAfter  :: String
  , guess      :: Maybe String
  }

_codeBefore :: Lens' State String
_codeBefore = lens _.codeBefore (_ { codeBefore = _ })

_codeAfter :: Lens' State String
_codeAfter = lens _.codeAfter (_ { codeAfter = _ })

_guess :: Lens' State (Maybe String)
_guess = lens _.guess (_ { guess = _ })

type Slot = Either3 Unit Unit Unit

cmSlotBefore :: ChildPath CM.Query ChildQuery Unit Slot
cmSlotBefore = cp1

cmSlotAfter :: ChildPath CM.Query ChildQuery Unit Slot
cmSlotAfter = cp2

cmSlotPatched :: ChildPath CM.Query ChildQuery Unit Slot
cmSlotPatched = cp3

type Input = Unit

type Message = Void

handleCodeMirrorBefore :: CM.Message -> Maybe (Query Unit)
handleCodeMirrorBefore = case _ of
  CM.Updated code -> Just $ H.action $ BeforeUpdated code

handleCodeMirrorAfter :: CM.Message -> Maybe (Query Unit)
handleCodeMirrorAfter = case _ of
  CM.Updated code -> Just $ H.action $ AfterUpdated code

handleCodeMirrorPatched :: CM.Message -> Maybe (Query Unit)
handleCodeMirrorPatched = pure Nothing

type ChickEffects e =
  ( ajax    :: AX.AJAX
  , avar    :: AVAR
  , console :: CONSOLE
  | e)

type Render m = H.ParentHTML Query ChildQuery Slot m

affjaxQuery ::
  ∀ a e.
  Requestable a =>
  String -> Maybe a -> AX.Affjax e String
affjaxQuery url content = AX.affjax
  { method          : Left POST
  , url
  , headers         : []
  , content
  , username        : Nothing
  , password        : Nothing
  , withCredentials : false
  }

guessText :: Maybe String -> String
guessText = case _ of
  Nothing -> "No guess!"
  Just g  -> g

render :: ∀ m e. MonadAff (ChickEffects e) m => State -> Render m
render { codeBefore, codeAfter, guess } =
  HH.div [ style $ do
              flexRow
              CSS.height    $ CSS.fromString "100%"
              --CSS.minHeight $ CSS.fromString "100%"
         ]
  $
  [ HH.slot'
    cmSlotBefore
    unit
    CM.codeMirrorComponent { code : codeBefore }
    handleCodeMirrorBefore
  ]
  <>
  [ HH.slot'
    cmSlotAfter
    unit
    CM.codeMirrorComponent { code : codeAfter }
    handleCodeMirrorAfter
  ]
  <>
  [ HH.slot'
    cmSlotPatched
    unit
    CM.codeMirrorComponent { code : guessText guess }
    handleCodeMirrorPatched
  ]

mkGuessInput :: State -> Json
mkGuessInput { codeBefore, codeAfter } =
  fromObject $ SM.fromFoldable
  [ Tuple "before" (fromString codeBefore)
  , Tuple "after"  (fromString codeAfter)
  ]

type DSL = H.ParentDSL State Query ChildQuery Slot Message

set :: ∀ e m a. MonadAff (ChickEffects e) m => Lens' State a -> a -> DSL m Unit
set lens value = H.modify $ over lens $ const value

updateGuess :: ∀ e m. MonadAff (ChickEffects e) m => DSL m Unit
updateGuess = do
  s <- H.get
  { status, response } <- H.liftAff $ do
    affjaxQuery "chickGuess" (Just (stringify (mkGuessInput s)))
  case status of
    StatusCode 200 -> do
      case runExcept (decodeJSON response) of
        Left e -> H.liftEff $ log "Could not decode JSON"
        Right r -> do
          --H.liftEff $ log response
          set _guess $ Just r
          _ <- H.query' cmSlotPatched unit (H.action $ CM.SetValue r)
          pure unit
    _ -> do
      H.liftEff $ log "bad status code :("

eval :: ∀ e m. MonadAff (ChickEffects e) m => Query ~> DSL m
eval = case _ of

  BeforeUpdated bef next -> do
    set _codeBefore bef
    updateGuess
    pure next

  AfterUpdated aft next -> do
    set _codeAfter aft
    updateGuess
    pure next

chickComponent ::
  ∀ e m.
  MonadAff (ChickEffects e) m =>
  H.Component HH.HTML Query Input Message m
chickComponent =
  H.parentComponent
    { initialState : const { codeBefore : EX.codeBefore
                           , codeAfter  : EX.codeAfter
                           , guess      : Nothing
                           }
    , render
    , eval
    , receiver     : const Nothing
    }
