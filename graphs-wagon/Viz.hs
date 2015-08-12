{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE ViewPatterns    #-}
module Viz where

import           Control.Monad        (foldM)
import           Control.Monad.Writer (execWriter, tell)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr, match, matchAny)

import           Data.String.Interpolation (str)

-- | The small example graph I used for the diagrams in my blog post.
example :: Gr () ()
example = Graph.mkGraph nodes edges
  where nodes = [(x, ()) | x <- [1..7]]
        edges = [(x, y, ()) | x <- [1..7], y <- [1..7], x /= y, x - y `elem` [3..5]]

-- | The larger example graph I used for lightning talk slides.
example' :: Gr () ()
example' = Graph.mkGraph nodes edges
  where nodes = map (,()) [1..10]
        edges = [ (6, 1, ()), (5, 1, ()), (4, 1, ())
                , (7, 2, ()), (6, 2, ()), (5, 2, ())
                , (7, 3, ()), (6, 3, ()), (8, 3, ())
                , (7, 4, ()), (10, 4, ())
                , (8, 5, ()), (9, 5, ())
                , (9, 6, ())
                , (10, 9, ())
                , (8, 10, ())
                ]

blue = "#0b4faa"

lightBlue = "#90A9CB"

bothN :: Graph.Node -> Gr a b -> Gr a b -> Bool
bothN node g₁ g₂ = node `elem` Graph.nodes g₁ && node `elem` Graph.nodes g₂

bothE :: Graph.Edge -> Gr a b -> Gr a b -> Bool
bothE edge g₁ g₂ = edge `elem` Graph.edges g₁ && edge `elem` Graph.edges g₂

nodeStyle :: Graph.Node -> Graph.Node -> Gr a b -> Gr a b -> String
nodeStyle n node full rest
  | n == node          = [str|[fontname="Junction" shape="doublecircle" color="$blue$" style="filled" fillcolor="$lightBlue$"]|]
  | bothN n full rest = [str|[fontname="Junction" shape="circle"]|]
  | otherwise         = [str|[fontname="Junction" shape="circle" color="white" style="filled" fillcolor="white" fontcolor="white"]|]

edgeStyle :: Graph.Edge -> Graph.Node -> Gr a b -> Gr a b -> String
edgeStyle (a, b) node full rest
  | (a == node && bothN b full rest) || (b == node && bothN a full rest) = [str|[color="$blue$" arrowhead="onormal"]|]
  | bothE (a, b) full rest = ""
  | otherwise              = [str|[color="white"]|]

decomposition full (_, node, _, _) rest = [str|
digraph fgl {
	margin = "0"
	page = "4"
	size = "4"
	ratio = "fill"
	#n in Graph.nodes full:$:n$ $nodeStyle n node full rest$|
        # 
        #(a, b) in Graph.edges full:$:a$ -> $:b$ $edgeStyle (a, b) node full rest$|
        #
}
|]

anyViz full (matchAny -> (ctx, graph)) = decomposition full ctx graph

nViz full n (match n -> (Just ctx, graph)) = decomposition full ctx graph

unlabeled graph = [str|
digraph fgl {
	margin = "0"
	page = "4"
	size = "4"
	ratio = "fill"
	#node in Graph.nodes graph:$:node$|
        # 
        #(a, b, l) in Graph.labEdges graph:$:a$ -> $:b$ |
        #
}                
|]

-- vizDfs :: (Show a, Show b) => Gr a b -> [String]
-- vizDfs full = execWriter . foldM (flip go) full $ dfs (ghead full) full
--   where go node (match node -> (Just ctx, graph)) = do
--           tell [decomposition full ctx graph]
--           return graph
--         go _ graph = return graph
