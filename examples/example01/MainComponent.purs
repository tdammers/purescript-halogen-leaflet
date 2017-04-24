module MainComponent
where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LeafletComponent as LC
import Control.Monad.Aff
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Leaflet (LEAFLET, LatLng, Zoom)
import Data.Tuple (Tuple (..))
import Util (formatGeo)

data Query a
  = HandleLeaflet LC.Message a

type State =
  { view :: Maybe (Tuple LatLng Zoom)
  }

data Slot = LeafletSlot
derive instance eqLeafletSlot :: Eq Slot
derive instance ordLeafletSlot :: Ord Slot

ui :: forall eff
    . H.Component
       HH.HTML
       Query
       Unit
       Void
       (Aff (avar :: AVAR, leaflet :: LEAFLET, err :: EXCEPTION | eff))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { view: Nothing
    }

  render :: State
         -> H.ParentHTML
              Query
              LC.Query
              Slot
              (Aff (avar :: AVAR, leaflet :: LEAFLET, err :: EXCEPTION | eff))

  render state =
    HH.div_
      [ HH.slot LeafletSlot (LC.ui "slippy_map") unit (HE.input HandleLeaflet)
      , HH.div
        [ HP.class_ $ H.ClassName "overlay"
        ]
        case state.view of
          Nothing -> []
          Just (Tuple { lat: lat, lng: lng } zoom) ->
            [ HH.div
              [ HP.class_ $ H.ClassName "panel"
              ]
              [ HH.div_ [ HH.text $ "lat: " <> formatGeo "N" "S" lat ]
              , HH.div_ [ HH.text $ "lng: " <> formatGeo "E" "W" lng ]
              , HH.div_ [ HH.text $ "zoom: " <> show zoom ]
              ]
            ]
      ]

  eval :: Query
       ~> H.ParentDSL
            State
            Query
            LC.Query
            Slot
            Void
            (Aff (avar :: AVAR, leaflet :: LEAFLET, err :: EXCEPTION | eff))
  eval = case _ of
    HandleLeaflet msg next -> do
      v <- H.query LeafletSlot $ H.request LC.GetView
      H.modify _ { view = join v }
      pure next

