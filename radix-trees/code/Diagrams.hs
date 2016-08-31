{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Diagrams where

import           Control.Monad.State

import           Data.Function                (on)
import           Data.List                    (intercalate)
import           Data.Tree                    (Tree)
import qualified Data.Tree                    as Tree

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

import qualified Trie

circleLabel (_, Nothing) = circle 0.25 # fc white
circleLabel (c, Just n)  = text (show n) <> (circle 0.75 # fc white)

nodeSpacing, charWidth :: (Floating n, Ord n) => n
nodeSpacing = 4
charWidth   = 0.25

ellipseLabel (c, _) = text c <> ellipseXY (w c / 2) 1 # fc white
  where w label = fromIntegral (length label) * charWidth + nodeSpacing - 1

show' = intercalate "" . map show

tree :: (Show a, Eq a) => [[a]] -> Tree ([a], Maybe Integer) -> Diagram B
tree highlights = renderTree' circleLabel edge . symmLayout' settings
  where settings = with & slHSep .~ 2.5 & slVSep .~ 4
        edge ((k₁, _), p₁) ((k₂, _), p₂) = label <> line
          where line = strokeLocTrail $ fromVertices [p₁, p₂]
                letter = take 1 $ drop (length k₁) k₂
                label = text (show' letter)
                  # moveTo fudged
                  # if (k₁ ++ letter) `elem` highlights
                    then fc blue
                    else id
                ((x₁, y₁), (x₂, y₂)) = (unp2 p₁, unp2 p₂)
                mid = (p₂ + p₁) / 2
                slope' = (x₂ - x₁) / (y₂ - y₁)
                fudged | x₁ /= x₂  = mid - p2 (signum slope' * sqrt (abs slope'), 0.2)
                       | otherwise = mid + p2 (0.4, -0.2)

hl t s = treeDiagram
  where treeDiagram :: Diagram B
        treeDiagram = diagram # translateY 10
                              # pad 1.1
                              # bg white
                              # font "DejaVu Sans Mono"
        diagram = tree highlights $ Trie.toTree t
        highlights = s
