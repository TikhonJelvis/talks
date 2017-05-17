{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module DistTree where

import Data.Tree (Tree (..))
import qualified Data.Tree as Tree

import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree



circleLabel (_, Nothing) = circle 0.01 # fc black
circleLabel (c, Just n)  = text n # translateY (-0.4)


tree = tree' circleLabel

tree' customLabel highlights = renderTree' customLabel edge . symmLayout' settings
  where settings = with & slHSep .~ 3 & slVSep .~ 2
        edge ((k₁, _), p₁) ((k₂, _), p₂) = label <> line
          where line = strokeLocTrail (fromVertices [p₁, p₂]) # lw 6
                label = text k₂
                  # moveTo fudged
                ((x₁, y₁), (x₂, y₂)) = (unp2 p₁, unp2 p₂)
                mid = (p₂ + p₁) / 2
                slope' = (x₂ - x₁) / (y₂ - y₁)
                fudged | x₁ /= x₂  = mid - p2 (signum slope' * sqrt (abs slope'), 0.2)
                       | otherwise = mid + p2 (0.4, -0.2)


drawTree fontSize t = treeDiagram
  where treeDiagram :: Diagram B
        treeDiagram = diagram # translateY 1
                              # pad 1.5
                              # font "DejaVu Sans Mono"
                              # fontSizeN fontSize
        diagram = tree [] t

flipTree = Node ("0.5", Nothing) [Node ("0.5", Just "H") [], Node ("0.5", Just "T") []]

flipTree' = Node ("0.5", Nothing) [Node ("0.1", Just "H") [], Node ("0.9", Just "T") []]

nested = Node ("1", Nothing) [flipTree, flipTree']

flattened = Node ("1", Nothing) [Node ("0.25", Just "H") [], Node ("0.25", Just "T") [], Node ("0.05", Just "H") [], Node ("0.45", Just "T") []]

final = Node ("0.5", Nothing) [Node ("0.3", Just "H") [], Node ("0.7", Just "T") []]

toSVG width path fs tree = renderSVG path (mkWidth width) $ drawTree fs tree
