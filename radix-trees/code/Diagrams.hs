{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Diagrams where

import           Control.Monad.State

import           Data.Function                (on)
import           Data.List                    (intercalate)
import           Data.Tree                    (Tree (..))
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

ellipseLabel (show -> c, _) = text c <> ellipseXY (w c / 2) 1 # fc white
  where w label = fromIntegral (length label) * charWidth + nodeSpacing - 1

class MyShow a where
  myShow :: a -> String

instance {-# OVERLAPPABLE #-} Show a => MyShow a where
  myShow = show

instance MyShow Char where
  myShow = (:"") 

show' = intercalate "" . map myShow

tree = tree' circleLabel

tree' customLabel highlights = renderTree' customLabel edge . symmLayout' settings
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
                              # font "DejaVu Sans Mono"
        diagram = tree highlights $ Trie.toTree t
        highlights = s

data C = C [Int] | V Int | None

clabel None   = circleLabel (undefined, Nothing :: Maybe Int)
clabel (V i)  = circleLabel (undefined, Just i)
clabel (C is) = text (intercalate "" $ show <$> is) <> rect (w is) 2 # bg white
  where charWidth = 0.8
        w ls = max 1 $ fromIntegral (length ls) * charWidth

ctree :: Tree (Int, C)
ctree = Node (undefined, C [0, 0, 0]) [Node (0, None) [Node (1, (V 1)) []], Node (1, None) [Node (1, (V 2)) []]]

ctree₂ = Node (undefined, None) [sub₁, sub₂]
  where sub₁ = Node (0, C [0, 0]) [Node (0, None) [Node (1, (V 1)) []], Node (1, None) [Node (1, (V 2)) []]]
        sub₂ = Node (1, None) [Node (0, None) [Node (0, None) [Node (1, None) [Node (1, V 3) []]]]]

drawCTree :: Tree ((Int, C)) -> Diagram B
drawCTree ctree = diagram # pad 1.1
                          # font "DejaVu Sans Mono"
  where diagram = renderTree' label edge $ symmLayout' settings ctree
        settings = with & slHSep .~ 2.5 & slVSep .~ 4

        label (k, l) = clabel l
        edge ((_, _), p₁) ((key, _), p₂) = edgeLabel <> line
          where line = strokeLocTrail $ fromVertices [p₁, p₂]
                edgeLabel = text (show key)
                  # moveTo fudged
                ((x₁, y₁), (x₂, y₂)) = (unp2 p₁, unp2 p₂)
                mid = (p₂ + p₁) / 2
                slope' = (x₂ - x₁) / (y₂ - y₁)
                fudged | x₁ /= x₂  = mid - p2 (signum slope' * sqrt (abs slope'), 0.2)
                       | otherwise = mid + p2 (0.4, -0.2)

data CV = CV [Int] | VC [Int] Int

cvtree = Node (undefined, CV []) [sub₁, sub₂]
  where sub₁ = Node (0, CV [0, 0]) [Node (0, VC [0, 1] 1) [], Node (0, VC [0, 1] 2) []]
        sub₂ = Node (1, VC [0, 0, 1, 1] 3) []

cvlabel (CV [])   = circleLabel (undefined, Nothing :: Maybe Int)
cvlabel (CV is)   = clabel (C is)
cvlabel (VC is i) = cs ||| v
  where cs = text (intercalate "" $ show <$> is) <> rect (w is) 2 # bg white
        v = text (show i) <> rect 1 2 # bg white

        charWidth = 0.8
        w ls = max 1 $ fromIntegral (length ls) * charWidth

drawCVTree :: Tree ((Int, CV)) -> Diagram B
drawCVTree ctree = diagram # pad 1.1
                           # font "DejaVu Sans Mono"
  where diagram = renderTree' label edge $ symmLayout' settings ctree
        settings = with & slHSep .~ 3.25 & slVSep .~ 4

        label :: (a, CV) -> Diagram B
        label (_, l) = cvlabel l
        edge ((_, _), p₁) ((key, _), p₂) = edgeLabel <> line
          where line = strokeLocTrail $ fromVertices [p₁, p₂]
                edgeLabel = text (show key)
                  # moveTo fudged
                ((x₁, y₁), (x₂, y₂)) = (unp2 p₁, unp2 p₂)
                mid = (p₂ + p₁) / 2
                slope' = (x₂ - x₁) / (y₂ - y₁)
                fudged | x₁ /= x₂  = mid - p2 (signum slope' * sqrt (abs slope'), 0.2)
                       | otherwise = mid + p2 (0.4, -0.2)

data W = W Int

wLabel (W 1) = centerX $ circle 1 # fc white
wLabel (W i) = centerX $ foldl1 (|||) (replicate i node)
  where node = (circle 0.2 # fc black) <> (square 2 # bg white)

smallTree = n [n [n [l, l], l], n [n [l, l], l]]
  where n c = Node (W 2) c
        l   = Node (W 1) []

largerTree = n [n [l, l], l, n [l, l], l]
  where n c = Node (W 4) c
        l   = Node (W 1) []

drawWTree :: Tree W -> Diagram B
drawWTree t = diagram # pad 1.1
  where diagram = renderTree wLabel (~~) $ symmLayout' settings t
        settings = with & slHSep .~ 7 & slVSep .~ 4
