module Colour(module Data.Colour, module Data.Colour.Names, module Data.Colour.SRGB, setSourceColour, setSourceColourAlpha, toColour, blendRGB) where

import Data.Colour hiding (Colour)
import Data.Colour.Names hiding (Colour)
import Data.Colour.SRGB hiding (Colour)
import qualified Data.Colour.SRGB as Colour
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import GHC.Word

import WidgetValue

colorToRGB :: Gtk.Color -> RGB Double
colorToRGB (Gtk.Color r g b) = RGB (f r) (f g) (f b)
  where f = (/ fromIntegral (maxBound :: Word16)) . fromIntegral

rgbToColor :: RGB Double -> Gtk.Color
rgbToColor (RGB r g b) = Gtk.Color (f r) (f g) (f b)
  where f = round . (* fromIntegral (maxBound :: Word16))

instance WidgetValue (RGB Double) ColorButton where
  makeWidget = do
    b <- colorButtonNew
    return (b, Just . colorToRGB <$> colorButtonGetColor b, colorButtonSetColor b . rgbToColor)

instance WidgetValue (Maybe (RGB Double)) HBox where
  makeWidget = maybeWidget

setSourceColour :: RGB Double -> Render ()
setSourceColour (RGB r g b) = setSourceRGB r g b

setSourceColourAlpha :: RGB Double -> Double -> Render ()
setSourceColourAlpha (RGB r g b) a= setSourceRGBA r g b a

toColour :: RGB Double -> Colour.Colour Double
toColour (RGB r g b) = sRGB r g b

blendRGB :: Double -> RGB Double -> RGB Double -> RGB Double
blendRGB x a b = toSRGB $ blend x (toColour a) (toColour b)
