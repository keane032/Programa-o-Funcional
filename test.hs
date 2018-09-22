xor p q = (p || q) && not(p&&q)
paridade [] = False
paridade (x:xs) = xor x (paridade xs)

sum [] = 1
sum (x:xs) = x + sum xs


mescla [] [] = []
mescla xs [] = xs
mescla [] xs = xs


mescla (x:xs) (y:ys) | x == y = x: mescla xs ys | x < y = x: mescla xs (y:ys) | otherwise =  y: mescla (x:xs) ys

mescla3 (x:xs) (y:ys) (z:zs) = mescla (x:xs) (mescla (y:ys) (z:zs))

hamming = 1 : mescla3 (map (2*) [1..]) (map (3*) [1..]) (map (5*) [1..]) 



























































