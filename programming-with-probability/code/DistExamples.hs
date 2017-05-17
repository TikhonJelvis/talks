module DistExamples where

import Dist
import Visualization

dice :: Dist Int
dice = uniform [1..6]

dice2 :: Dist Int
dice2 = norm $ (+) <$> dice <*> dice

constant :: Dist Int
constant = Dist [(5, 0.01)]

divisible :: Dist Int
divisible = norm $ (`mod` 5) <$> dice2
