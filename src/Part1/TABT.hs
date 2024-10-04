module Part1.TABT where

-- Ex. 1.4 i Prove by Curry-Howard (a^b)^c = a^(b*c)
-- A: they are curry and uncurry
f :: (b -> c -> a) -> (b, c) -> a
f i (b, c) = i b c

g :: ((b, c) -> a) -> b -> c -> a
g i b c = i (b, c)

-- Ex. 1.4 ii Prove exponent law: a^b * a^c = a^(b+c)
h :: (b -> a, c -> a) -> Either b c -> a
h (i1, _) (Left b) = i1 b
h (_, i2) (Right c) = i2 c

k :: (Either b c -> a) -> (b -> a, c -> a)
k i = (i . Left, i . Right)

-- Ex 1.4 iii Prove: (a*b)^c = a^c * b^c
l :: (c -> (a, b)) -> (c -> a, c -> b)
l i = (fst . i, snd . i)

m :: (c -> a, c -> b) -> c -> (a, b)
m (i1, i2) c = (i1 c, i2 c)
