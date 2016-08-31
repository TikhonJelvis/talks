module Trie where

import           Prelude hiding (words)

import           Data.List (intercalate, sort)
import           Data.Tree (Tree)
import qualified Data.Tree as Tree

-- | A 'String' → a map implement as a trie that reads keys in
-- character by character.
data Trie k a = Node (Maybe a) [(k, Trie k a)] deriving (Show, Eq)

empty :: Trie k a
empty = Node Nothing []

get :: Eq k => [k] -> Trie k a -> Maybe a
get [] (Node x _)            = x
get (c:cs) (Node _ children) = lookup c children >>= get cs

insert :: Eq k => [k] -> a -> Trie k a -> Trie k a
insert [] x (Node _ children)       = Node (Just x) children
insert (c:cs) x (Node val children) = Node val $ on c (insert cs x) children
  where on c f [] = [(c, f $ Node Nothing [])]
        on c f ((c', x) : cs)
          | c == c'   = (c, f x) : cs
          | otherwise = (c', x) : on c f cs

fromList :: Eq k => [([k], a)] -> Trie k a
fromList = foldl (flip $ uncurry insert) empty

toTree :: Trie k a -> Tree ([k], Maybe a)
toTree (Node val children) = Tree.Node ([], val) $ go [] <$> children
  where go soFar (char, Node val children') = Tree.Node (soFar', val) $ go soFar' <$> children'
          where soFar' = soFar ++ [char]

t₁ = fromList [("abc", 1), ("def", 2), ("abcd", 3), ("acde", 4), ("acef", 5), ("acfe", 6)]

t₂ = fromList $ zip ["a", "b", "ab", "abc", "ac", "baa", "bb", "bab", "bac", "bbb", "bbc"] [0..]

t₄ = fromList $ zip ["a", "b", "c"] [5, 6, 7]

words = sort [ "cat"
             , "bat"
             , "cats"
             , "at"
             , "ate"
             , "sate"
             , "bate"
             ]

t₃ = fromList $ zip words [0..]

bits = [[0,0,0], [0,0,1], [0,1,0], [0,1,1], [1,0,0], [1,0,1], [1,1,0], [1,1,1]]

t₆ = fromList $ zip bits [0..]

t₇ = fromList $ [([0, 0, 0, 1, 1], 1), ([0, 0, 0, 0, 1], 2)]

data Compressed = C [Int] | V Int deriving Eq

instance Show Compressed where
  show (C is) = intercalate "" $ show <$> is
  show (V i) = show i

t₈ = fromList [([[0,0,0,0], [1, 1]], 1), ([[0,0,0,0], [0, 1]], 2)]
