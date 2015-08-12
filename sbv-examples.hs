import Data.SBV

formula = do x :: SInteger <- exists "x"
             y :: SInteger <- exists "y"
             constrain $ x^2 + y^2 .== 25
             return $ 3 * x + 4 * y .== 0
  
