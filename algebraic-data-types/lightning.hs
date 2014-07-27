{-# LANGUAGE TypeOperators #-}

type a × b = (a, b)
type a + b = Either a b

data Void

f :: Either a a -> (Bool, a)
f (Left a) = (True, a)
f (Right a) = (False, a)

g :: (Bool, a) -> Either a a
g (True, a) = Left a
g (False, a) = _

data Nat = Z | S Nat
