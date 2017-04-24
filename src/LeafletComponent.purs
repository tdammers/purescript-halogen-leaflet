module LeafletComponent where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.AVar (AVAR)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (maybe, fromMaybe, Maybe (..))
import Data.Array as Array
import Data.Either (either, Either (..))
import Control.Monad.Eff.Exception (throw, EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Math as Math
import Leaflet as Leaflet
import Leaflet (LEAFLET, LatLng, MouseEvent)
import Data.Tuple (Tuple (..))

type State =
  { leaflet :: Maybe Leaflet.Map
  , tileLayers :: Array Leaflet.Layer
  }

data Query a
  = Initialize a
  | Finalize a
  | AddTileLayer String a
  | GetView (Maybe (Tuple Leaflet.LatLng Leaflet.Zoom) -> a)
  | HandleMove (H.SubscribeStatus -> a)
  | HandleZoom (H.SubscribeStatus -> a)
  | HandleClick MouseEvent (H.SubscribeStatus -> a)
  | HandleDblClick MouseEvent (H.SubscribeStatus -> a)
  | HandleMouseMove MouseEvent (H.SubscribeStatus -> a)

data Message
  = Initialized
  | Moved
  | Zoomed
  | Clicked MouseEvent
  | DblClicked MouseEvent
  | MouseMoved MouseEvent

ui =
  H.lifecycleComponent
    { initialState: const initialLeaflet
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    }

initialLeaflet =
  { leaflet: Nothing
  , tileLayers: []
  }

render state =
  HH.div
    [ HP.ref (H.RefLabel "leaflet")
    , HP.prop (HC.PropName "id") "leaflet"
    ]
    [ ]

eval :: forall eff
      . Query
     ~> H.ComponentDSL State Query Message
        (Aff (avar :: AVAR, leaflet :: LEAFLET, err :: EXCEPTION | eff))
eval (Initialize next) = do
  Tuple m l <- H.liftEff do
    m <- Leaflet.map
      "leaflet"
      (Leaflet.latlng 52.0 4.0)
      7
    l <- Leaflet.tileLayer("//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")
    Leaflet.addLayer l m
    pure $ Tuple m l

  H.modify $ \state ->
    state
      { tileLayers = [l]
      , leaflet = Just m
      }

  H.subscribe $ H.eventSource_
    (Leaflet.onZoom m)
    (H.request HandleZoom)
  H.subscribe $ H.eventSource
    (Leaflet.onMove m)
    (\latlng -> Just (H.request HandleMove))
  H.subscribe $ H.eventSource
    (Leaflet.onClick m)
    (\mouseEvent -> Just (H.request (HandleClick mouseEvent)))
  H.subscribe $ H.eventSource
    (Leaflet.onDblClick m)
    (\mouseEvent -> Just (H.request (HandleDblClick mouseEvent)))
  H.subscribe $ H.eventSource
    (Leaflet.onMouseMove m)
    (\mouseEvent -> Just (H.request (HandleMouseMove mouseEvent)))

  H.raise Initialized
  pure next

eval (AddTileLayer url next) = do
  H.gets (_.leaflet) >>=
    case _ of
      Nothing -> pure unit
      Just m -> do
        l <- H.liftEff do
          l <- Leaflet.tileLayer(url)
          Leaflet.addLayer l m
          pure l
        H.modify (\state ->
            state
              { tileLayers = Array.snoc state.tileLayers l
              }
          )
  pure next

eval (HandleMove reply) = do
  H.raise Moved
  pure $ reply H.Listening
eval (HandleZoom reply) = do
  H.raise Zoomed
  pure $ reply H.Listening
eval (HandleClick mouseEvent reply) = do
  H.raise $ Clicked mouseEvent
  pure $ reply H.Listening
eval (HandleDblClick mouseEvent reply) = do
  H.raise $ DblClicked mouseEvent
  pure $ reply H.Listening
eval (HandleMouseMove mouseEvent reply) = do
  H.raise $ MouseMoved mouseEvent
  pure $ reply H.Listening
eval (Finalize next) = do
  pure next
eval (GetView reply) = do
  H.gets (_.leaflet) >>= maybe
    (pure $ reply Nothing)
    (\m -> do
      view <- H.liftEff do
        latlng <- Leaflet.getCenter m
        zoom <- Leaflet.getZoom m
        pure $ Tuple latlng zoom
      pure $ reply (Just view)
    )
