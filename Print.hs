module Print (printSampleNote) where

import Common

import Data.Time
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk
import Control.Monad

import System.Process

inch = (*) 72
scalingFactor = 3

printAt x y text =
    do moveTo (x / scalingFactor) (y / scalingFactor)
       layout <- createLayout text
       showLayout layout

drawSampleNote src day month year sampleNum grainType  = do
  setSourcePixbuf src 0 0
  paint

  setSourceRGB 0 0 0
  scale scalingFactor scalingFactor
  scale 0.36330608537693004 0.36330608537693004
  printAt 1122 1554 "x"

  printAt 375 1560 day
  printAt 512 1560 month
  printAt 681 1560 year
  printAt 432 3070 sampleNum 

{-
  when (grainType == Ohra) $
       printAt 174 1240 "x"

  when (grainType == Kaura) $
       printAt 416 1240 "x"

  when (grainType == Seka) $
       printAt 664 1240 "x"
-}

printSampleNote src time sampleNum grainType = do
        initGUI
        let tempFileName = "/tmp/foo.ps"
            (year, month, day) = toGregorian . utctDay $ time
	withPSSurface tempFileName 800 1216 $ \surface -> renderWith surface $
                                                          drawSampleNote src (show day) (show month) (show year) sampleNum grainType
	h <- runProcess "lpr" ["-o","PrintoutMode=Draft","-o", "fit-to-page", tempFileName] Nothing Nothing Nothing Nothing Nothing
	e <- waitForProcess h
	print e
