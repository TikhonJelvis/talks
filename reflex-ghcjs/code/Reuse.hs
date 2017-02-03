{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Reuse where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.BoundingBox
import Diagrams.Prelude hiding (arrow)
import Diagrams.TwoD.Arrow hiding (arrow)

import Graphics.SVGFonts

-- * Diagrams

bgColor = sRGB24 0xC0 0xD9 0xFB

textColor = sRGB24 0x05 0x2d 0x69

-- | The default architecture: low code reuse.
noShare = centerXY boxes `atop` background
  where boxes = arrows <> backend <> frontend

        backend = textBox "Backend"   # translate (r2 (0, 0))
               <> textBox "HTTP API"  # translate (r2 (2, -3.5))
               <> textBox "Types"     # translate (r2 (4, -7))

        frontend = textBox "UI"       # translate (r2 (16, 0))
                <> textBox "Types"    # translate (r2 (14, -3.5))
                <> textBox "HTTP API" # translate (r2 (12, -7))

        arrows = arrow (p2 (0.3, -1.2)) (p2 (1.7, -2.3))
              <> arrow (p2 (2.3, -4.7)) (p2 (3.7, -5.8))

              <> arrow (p2 (13.9, -2.3)) (p2 (15.3, -1.2))
              <> arrow (p2 (11.9, -5.8)) (p2 (13.3, -4.7))

              <> arrow (p2 (7, -7.2)) (p2 (9, -7.2))

sharedTypes = centerXY boxes `atop` background
  where boxes = arrows <> backend <> frontend <> types

        backend = textBox "Backend"   # translate (r2 (0, 0))
               <> textBox "HTTP API" # translate (r2 (2, -3.5))

        frontend = textBox "UI"        # translate (r2 (13.5, 0))
                <> textBox "HTTP API" # translate (r2 (11.5, -3.5))

        types = textBox "Types" # translate (r2 (7, -7))

        arrows = arrow (p2 (0.3, -1.2)) (p2 (1.7, -2.3))
              <> arrow (p2 (2.3, -4.7)) (p2 (5.7, -5.8))

              <> arrow (p2 (11.9, -2.3)) (p2 (13.5, -1.2))
              <> arrow (p2 (8.2, -5.8)) (p2 (11.6, -4.7))

sharedInterface = centerXY boxes `atop` background
  where boxes = arrows <> backend <> frontend <> interface <> types

        backend = textBox "Backend" # translate (r2 (0, 0))

        frontend = textBox "UI" # translate (r2 (13.5, 0))

        interface = textBox "HTTP API" # translate (r2 (7, -3.5))
        
        types = textBox "Types" # translate (r2 (7, -7))

        arrows = arrow (p2 (0, -1.2)) (p2 (4, -5))
              <> arrow (p2 (10, -5)) (p2 (13.5, -1.2))
              <> arrow (p2 (6, -4.8)) (p2 (6, -5.8))
              <> arrow (p2 (8, -5.8)) (p2 (8, -4.8))

arrow p₁ p₂ = arrowBetween' opts p₁ p₂
  where opts = with & arrowHead .~ tri
                    & headLength .~ small
                    & headStyle %~ lw 5
                    & shaftStyle %~ lw 10

background = rect 25 15 # fc white # lc white


-- * Utilities

-- | Utilities based on cube diagram example by Chris Mears
-- (<http://projects.haskell.org/diagrams/gallery/SymmetryCube.html>).

text' :: String -> Double -> Diagram B
text' s n = textSVG_ (textOpts n) s # fc textColor # lw none
  where textOpts n = TextOpts lin2 INSIDE_H KERN False 1 n

padAmount = 0.5

textBox str = (centerXY (text' str 1) `atop` rect 4 2) # fc bgColor
                                            # lc textColor
                                            # lw 5

box innards padding =
    let padded =                  strutY padding
                                       ===
             (strutX padding ||| centerXY innards ||| strutX padding)
                                       ===
                                  strutY padding
        height = diameter (r2 (0,1)) padded
        width  = diameter (r2 (1,0)) padded
    in centerXY innards <> roundedRect width height 0.1
