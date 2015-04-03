{-# LANGUAGE DeriveFunctor #-}

import qualified Data.Tree as T


data Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> (Fix f -> a)
fold fa (Fix fs) = fa $ fmap (fold fa) fs

data ListF a r = Cons a r | Nil deriving (Show, Functor)
type List a = Fix (ListF a)

sum' :: List Int -> Int
sum' = fold fa
  where fa (Cons a b) = a + b
        fa Nil        = 0

fromList :: [a] -> List a
fromList []     = Fix Nil
fromList (a:as) = Fix (Cons a (fromList as))

toList :: List a -> [a]
toList (Fix Nil)         = []
toList (Fix (Cons a as)) = a : toList as

data TreeF a r = Node a [r] deriving (Functor)
type Tree a = Fix (TreeF a)

fromTree :: T.Tree a -> Tree a
fromTree (T.Node x ts) = Fix (Node x (map fromTree ts))

treeSum :: Tree Int -> Int
treeSum = fold fa
  where fa (Node x ts) = x + sum ts
