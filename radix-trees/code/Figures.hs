{-# LANGUAGE NoMonomorphismRestriction #-}
module Figures where

import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Size

import Diagrams
import qualified Trie as Trie

main = do renderHl "tree.svg" Trie.t₂ []
          renderHl "b.svg" Trie.t₂ ["b"]
          renderHl "b→ba.svg" Trie.t₂ ["b", "ba"]
          renderHl "b→ba→bab.svg" Trie.t₂ ["b", "ba", "bab"]

          renderSmall "prefix-ba.svg" Trie.t₄

          renderHl "binary.svg" Trie.t₆ []

          renderTall "paths-waste.svg" Trie.t₇ []

          renderCTree 500 "compressed.svg" ctree
          renderCTree 700 "compressed2.svg" ctree₂

          renderCVTree 500 "compressed-leaves.svg" cvtree

          renderSVG "wtrees.svg" (mkWidth 1200) $
            drawWTree smallTree ||| drawWTree largerTree

renderTree size path trie highlights = 
  renderSVG path size $ hl trie highlights

renderHl = renderTree $ mkWidth 550

renderTall = renderTree $ mkHeight 500

renderSmall path trie = renderSVG path size $ treeDiagram
  where size = mkWidth 400

        treeDiagram :: Diagram B
        treeDiagram = tree [] (Trie.toTree trie) # pad 1.1 # font "DejaVu Sans Mono"

renderCTree height path trie = renderSVG path size $ drawCTree trie
  where size = mkHeight height

renderCVTree height path trie = renderSVG path size $ drawCVTree trie
  where size = mkHeight height
