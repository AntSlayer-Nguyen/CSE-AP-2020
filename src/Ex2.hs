-- Return all pythagoras triples (x^2 + y^2 = z^2), x,y,z <= n -------------------
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a,b,c) |a <- [1..n], b <- [1..n], c <- [(max a b)..n] ,a*a + b*b == c*c]

-- Return all factors of a integer -----------------------------
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0] 

-- Return all perfect numbers in a given range <= n -------------
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]



