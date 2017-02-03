module BvE where

import Control.Lens

import Data.Colour
import Data.Colour.SRGB
import Data.Default.Class (def)

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams

out = renderableToFile def

bgColor = sRGB24 0xC0 0xD9 0xFB

textColor = sRGB24 0x05 0x2d 0x69

behavior = toRenderable layout
  where layout = def & layout_plots .~ [toPlot b₁]
                     & layout_background .~ FillStyleSolid (opaque bgColor)
                     & layout_x_axis . laxis_title .~ "time"
                     & layout_x_axis . laxis_title_style . font_size .~ 32
                     & layout_x_axis . laxis_generate .~ minimal (0, 6)
                     & layout_y_axis . laxis_title .~ "value"
                     & layout_y_axis . laxis_title_style . font_size .~ 32
                     & layout_y_axis . laxis_generate .~ minimal (-5, 4.5)

        minimal scaled xs =
          scaledAxis def scaled xs & axis_ticks .~ []
                                   & axis_labels .~ []
                                   & axis_grid .~ []
        
        b₁ = def & plot_lines_values .~ [fs [0,0.05..7]]
                 & plot_lines_style . line_color .~ opaque textColor
                 & plot_lines_style . line_width .~ 8
        
        fs :: [Double] -> [(Double, Double)]
        fs xs = [(x, f x) | x <- xs]
        f x = sin x + cos (2 * x) + sin (3 * x - 2)

event = toRenderable layout
  where layout = def & layout_plots .~ [toPlot b₁]
                     & layout_background .~ FillStyleSolid (opaque bgColor)
                     & layout_x_axis . laxis_title .~ "time"
                     & layout_x_axis . laxis_title_style . font_size .~ 32
                     & layout_x_axis . laxis_generate .~ minimal (0, 6)
                     & layout_y_axis . laxis_title .~ "value"
                     & layout_y_axis . laxis_title_style . font_size .~ 32
                     & layout_y_axis . laxis_generate .~ minimal (-5, 4.5)

        minimal scaled xs =
          scaledAxis def scaled xs & axis_ticks .~ []
                                   & axis_labels .~ []
                                   & axis_grid .~ []
        
        b₁ = def & plot_points_values .~ fs [0.2, 1.5, 2.2, 3, 3.7, 4.2, 5, 5.8, 6.8]
                 & plot_points_style .~ filledCircles 15 (opaque textColor)
        fs :: [Double] -> [(Double, Double)]
        fs xs = [(x, f x) | x <- xs]
        f x = sin x + cos (2 * x) + sin (3 * x - 2)


dynamic = toRenderable layout
  where layout = def & layout_plots .~ [toPlot b₁, toPlot b₂]
                     & layout_background .~ FillStyleSolid (opaque bgColor)
                     & layout_x_axis . laxis_title .~ "time"
                     & layout_x_axis . laxis_title_style . font_size .~ 32
                     & layout_x_axis . laxis_generate .~ minimal (0, 6)
                     & layout_y_axis . laxis_title .~ "value"
                     & layout_y_axis . laxis_title_style . font_size .~ 32
                     & layout_y_axis . laxis_generate .~ minimal (-5, 4.5)

        minimal scaled xs =
          scaledAxis def scaled xs & axis_ticks .~ []
                                   & axis_labels .~ []
                                   & axis_grid .~ []

        flatten []                         = [[]]
        flatten [(x, y)]                   = [[(x, y)]]
        flatten ((x₁, y₁) : (x₂, y₂) : ps) = [(x₁, y₁), (x₂ - 0.1, y₁)] : flatten ((x₂, y₂) : ps)
        
        b₁ = def & plot_lines_values .~ [(0, -1), (0.1, -1)] : flatten (fs [0.2, 1.5, 2.2, 3, 3.7, 4.2, 5, 5.8, 6.8])
                 & plot_lines_style . line_color .~ opaque textColor
                 & plot_lines_style . line_width .~ 8

        b₂ = def & plot_points_values .~ fs [0.2, 1.5, 2.2, 3, 3.7, 4.2, 5, 5.8, 6.8]
                 & plot_points_style .~ filledCircles 15 (opaque textColor)

        fs :: [Double] -> [(Double, Double)]
        fs xs = [(x, f x) | x <- xs]
        f x = sin x + cos (2 * x) + sin (3 * x - 2)
