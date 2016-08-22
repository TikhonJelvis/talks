{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Diagrams where

import           Data.Tree                    (Tree)
import qualified Data.Tree                    as Tree

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

import qualified Trie

toLabel (c, Nothing) = text c <> circle 1 # fc white
toLabel (c, Just n)  = text (c ++ " " ++ show n) <> (circle 1 # fc white)

nodeSpacing, charWidth :: (Floating n, Ord n) => n
nodeSpacing = 4
charWidth   = 0.25

toLabel' (c, _) = text c <> ellipseXY (w c / 2) 1 # fc white
  where w label = fromIntegral (length label) * charWidth + nodeSpacing - 1

tree = renderTree toLabel' (~~) . symmLayout' settings
  where settings = with & slHSep .~ 7 & slVSep .~ 4

main = mainWith treeDiagram
  where treeDiagram :: Diagram B
        treeDiagram = tree (Trie.toTree Trie.t₆) # translateY 10 # pad 1.1 # bg white
