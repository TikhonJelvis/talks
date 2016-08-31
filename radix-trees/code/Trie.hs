module Trie where

import           Prelude hiding (words)

import           Data.List (sort)
import           Data.Tree (Tree)
import qualified Data.Tree as Tree

-- | A 'String' → a map implement as a trie that reads keys in
-- character by character.
data Trie a = Node (Maybe a) [(Char, Trie a)] deriving (Show, Eq)

empty :: Trie a
empty = Node Nothing []

get :: String -> Trie a -> Maybe a
get "" (Node x _)            = x
get (c:cs) (Node _ children) = lookup c children >>= get cs

insert :: String -> a -> Trie a -> Trie a
insert "" x (Node _ children)       = Node (Just x) children
insert (c:cs) x (Node val children) = Node val $ on c (insert cs x) children
  where on c f [] = [(c, f $ Node Nothing [])]
        on c f ((c', x) : cs)
          | c == c'   = (c, f x) : cs
          | otherwise = (c', x) : on c f cs

fromList :: [(String, a)] -> Trie a
fromList = foldl (flip $ uncurry insert) empty

toTree :: Trie a -> Tree (String, Maybe a)
toTree (Node val children) = Tree.Node ("", val) $ go "" <$> children
  where go soFar (char, Node val children') = Tree.Node (soFar', val) $ go soFar' <$> children'
          where soFar' = soFar ++ [char]

t₁ = fromList [("abc", 1), ("def", 2), ("abcd", 3), ("acde", 4), ("acef", 5), ("acfe", 6)]

t₂ = fromList $ zip ["a", "b", "ab", "abc", "ac", "baa", "bb", "bab", "bac", "bbb", "bbc"] [0..]

words = sort [ "cat"
             , "bat"
             , "cats"
             , "at"
             , "ate"
             , "sate"
             , "bate"
             ]

t₃ = fromList $ zip words [0..]
