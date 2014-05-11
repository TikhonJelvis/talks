-- x = [x]

newtype Fix f a = Fix (f (Fix f a))

x = Fix [x, x]
