module Trie where

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

toTree :: Trie a -> Tree (String, Maybe a)
toTree (Node val children) = Tree.Node ("", val) $ go "" <$> children
  where go soFar (char, Node val children') = Tree.Node (soFar', val) $ go soFar' <$> children'
          where soFar' = soFar ++ [char]

t₁, t₂, t₃, t₄, t₅ :: Trie Integer
t₁ = insert "abc" 1 empty
t₂ = insert "def" 2 t₁
t₃ = insert "abcd" 3 t₂
t₄ = insert "acde" 4 t₃
t₅ = insert "acef" 5 t₄
t₆ = insert "acfe" 5 t₅
