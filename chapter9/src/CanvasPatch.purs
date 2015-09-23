module CanvasPatch where

import Control.Monad.Eff (Eff(..))
import Graphics.Canvas (Canvas(..), CanvasImageSource(..))

foreign import makeCanvasImageSource :: String -> forall eff. Eff (canvas :: Canvas | eff) CanvasImageSource
