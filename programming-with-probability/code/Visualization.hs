{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Visualization where

import Control.Lens
import Control.Monad (void)

import Data.Colour (AlphaColour, opaque, transparent)
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Default.Class (Default, def)
import Data.Monoid ((<>))

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)

import Dist

-- | The top-level title and axis labels for a graph.
data Titles = Titles { title, xAxis, yAxis :: String }

instance Default Titles where def = Titles "" "" ""

-- | A helper function for converting different numeric types to
-- doubles.
convert :: (Real n) => n -> Double
convert = fromRational . toRational

-- | A nice default color palette for plotting multiple different data
-- sets on one chart (currently with five colors).
colors :: [AlphaColour Double]
colors = map opaque [ blue, green, yellow, orange, red ]

plot :: (Show a, Ord a) => FilePath -> Dist a -> IO ()
plot name dist = toSVG renderable ("programming-with-probability/img/" <> name <> ".svg")
  where renderable = plotPDF titles dist
        titles = Titles name "x" "P(x)"

-- | Renders a 'Renderable' value to an SVG file.
toSVG :: Renderable a -> FilePath -> IO ()
toSVG diagram path = void $ renderableToFile def path diagram


bgColor = sRGB24 0xC0 0xD9 0xFB

textColor = sRGB24 0x05 0x2d 0x69

plotPDF :: forall a. (Show a, Ord a) =>
             Titles -> Dist a -> Renderable ()
plotPDF Titles {..} dist@(Dist ps) = toRenderable layout
  where layout = (def :: Layout PlotIndex Double)
                   & layout_x_axis . laxis_generate .~ autoIndexAxis xLabels
                   & layout_x_axis . laxis_title .~ xAxis
                   & layout_x_axis . laxis_title_style . font_size .~ 32
                   & layout_x_axis . laxis_style . axis_label_style . font_color .~ transparent
                   & layout_y_axis . laxis_generate .~ minimal (0, maxP)
                   & layout_y_axis . laxis_title .~ yAxis
                   & layout_y_axis . laxis_title_style . font_size .~ 32
                   & layout_plots .~ [ plotBars bars ]
        bars = def & plot_bars_values .~ addIndexes (map toVal $ dist ^.. probabilities)
                   & plot_bars_item_styles .~ repeat (FillStyleSolid $ opaque textColor, Nothing)
        toVal p = [fromRational $ toRational p]

        xLabels = show <$> dist ^.. events

        maxP = maximum $ snd <$> ps

        minimal scaled xs =
          scaledAxis def scaled xs & axis_ticks .~ []
                                   & axis_labels .~ []
                                   & axis_grid .~ []
