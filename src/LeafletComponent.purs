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
import Leaflet as L
import Leaflet (LEAFLET, LatLng, MouseEvent)
import Leaflet.TileLayer as TileLayer
import Data.Tuple (Tuple (..))
import Data.Map as Map
import Data.Map (Map)

type State =
  { leaflet :: Maybe L.Map
  , tileLayers :: Map Int L.Layer
  , ref :: LeafletRef
  , nextLayerID :: LayerID
  }

type LayerID = Int

data Query a
  = Initialize a
  | Finalize a
  | AddTileLayer String (Array TileLayer.Option) (Maybe LayerID -> a)
  | GetView (Maybe (Tuple L.LatLng L.Zoom) -> a)
  | GetRef (LeafletRef -> a)
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

type LeafletRef = String

ui ref =
  H.lifecycleComponent
    { initialState: const (initialLeaflet ref)
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    }

initialLeaflet :: LeafletRef -> State
initialLeaflet ref =
  { leaflet: Nothing
  , tileLayers: Map.empty
  , ref: ref
  , nextLayerID: 0
  }

render state =
  HH.div
    [ HP.ref (H.RefLabel state.ref)
    , HP.prop (HC.PropName "id") state.ref
    ]
    [ ]

eval :: forall eff
      . Query
     ~> H.ComponentDSL State Query Message
        (Aff (avar :: AVAR, leaflet :: LEAFLET, err :: EXCEPTION | eff))
eval (Initialize next) = do
  ref <- H.gets _.ref
  m <- H.liftEff do
    L.map ref (L.latlng 52.0 4.0) 7

  H.modify $ \state ->
    state
      { leaflet = Just m
      }

  H.subscribe $ H.eventSource_
    (L.onZoom m)
    (H.request HandleZoom)
  H.subscribe $ H.eventSource
    (void <<< L.onMove m)
    (\latlng -> Just (H.request HandleMove))
  H.subscribe $ H.eventSource
    (void <<< L.on L.Click m)
    (\mouseEvent -> Just (H.request (HandleClick mouseEvent)))
  H.subscribe $ H.eventSource
    (void <<< L.on L.DblClick m)
    (\mouseEvent -> Just (H.request (HandleDblClick mouseEvent)))
  H.subscribe $ H.eventSource
    (void <<< L.on L.MouseMove m)
    (\mouseEvent -> Just (H.request (HandleMouseMove mouseEvent)))

  H.raise Initialized
  pure next

eval (AddTileLayer url options reply) = do
  H.gets (_.leaflet) >>=
    case _ of
      Nothing ->
        pure $ reply Nothing
      Just m -> do
        l <- H.liftEff do
          l <- L.tileLayer url options
          L.addLayer l m
          pure l
        layerID <- H.gets _.nextLayerID
        H.modify (\state ->
            state
              { tileLayers = Map.insert layerID l state.tileLayers
              , nextLayerID = layerID + 1
              }
          )
        pure $ reply (Just layerID)

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
        latlng <- L.getCenter m
        zoom <- L.getZoom m
        pure $ Tuple latlng zoom
      pure $ reply (Just view)
    )
eval (GetRef reply) = do
  H.gets _.ref >>= pure <<< reply
