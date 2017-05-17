{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Dist where

import Control.Lens
import Control.Monad (void)

import Data.Colour (AlphaColour, opaque)
import Data.Colour.Names
import Data.Default.Class (Default, def)
import Data.List (genericLength)
import qualified Data.Map as Map

import Graphics.Rendering.Chart (Renderable)
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)


data Dist a = Dist { _values :: [(a, Double)] } deriving (Show, Functor)

makeLenses ''Dist

instance Applicative Dist where
  pure x = Dist [(x, 1)]

  Dist fs <*> Dist xs =
    Dist [(f x, pf * px) | (f, pf) <- fs, (x, px) <- xs]

instance Monad Dist where
  Dist xs >>= f = Dist $ do
    (x, px) <- xs
    [(y, px * py) | (y, py) <- _values $ f x]

-- | Group all instances of the same value together.
norm :: Ord a => Dist a -> Dist a
norm (Dist ps) =
  Dist $ Map.toList $ Map.fromListWith (+) ps


-- | A class for types that represent arbitrary discrete probability
-- distributions.
class Monad m => MonadDistribution m where
  weighted :: [(a, Double)] -> m a

  uniform :: [a] -> m a
  uniform xs = weighted $ uniformPs xs

  binomial :: Double -> Int -> m Int
  binomial = genericBinomial


instance MonadDistribution Dist where
  weighted = Dist

  binomial p n = norm $ genericBinomial p n

-- | A generic implementation of binomial that should work for any
-- type of distribution but may be woefully inefficient.
--
-- This is also the default implementation of 'binomial' for the
-- 'MonadDistribution' class.
genericBinomial :: MonadDistribution m => Double -> Int -> m Int
genericBinomial p = inner
  where inner n
          | n < 0  = error "binomial distribution expects non-negative number"
          | p < 0 || p > 1 = error "binomial distribution expects probability"
          | n == 0 = return 0
          | p == 0 = return 0
          | p == 1 = return n
          | n > 0  = do x <- weighted [(0, 1-p),(1,p)]
                        ((+) x) <$> inner (n - 1)
          | otherwise = undefined -- ERROR


uniformPs :: [a] -> [(a, Double)]
uniformPs xs = [(x, recip num) | x <- xs]
    where num = genericLength xs

-- | Traverse all the probabilities in a distribution.
probabilities :: Traversal' (Dist a) Double
probabilities = values . each . _2

-- | Traverse all the events in a distribution.
events :: Traversal' (Dist a) a
events = values . each . _1
