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

itemDemand = Dist [(0, 0.6), (1, 0.2), (2, 0.1), (3,0.05), (4, 0.025), (5, 0.025)]


