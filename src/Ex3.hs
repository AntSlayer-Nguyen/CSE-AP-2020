import           Prelude hiding (and, concat, replicate, (!!), elem)

--- and a list -----
and :: [Bool] -> Bool
and [a] = a
and (x:xs) = if (x == True) then and xs
             else False

--- concat lists into a given list -----
concat :: [[a]] -> [a]
concat [] = []
concat [a] = a
concat (x:l) = x ++ concat l

--- create a list of n same elements a ----
replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a = [a] ++ replicate (n-1) a

--- return index of an element in a list ----
(!!) :: [a] -> Int -> a
(!!) [] n = error "Wrong index!!"
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

--- return whether a list has element a -----
elem :: Eq a => a -> [a] -> Bool
elem n [] = False
elem n (x:xs) = if (n == x) then True
                else elem n xs

--- merge 2 lists ---------------------------
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if (x < y) then [x] ++ merge xs (y:ys)
                      else [y] ++ merge (x:xs) ys
--- seperate a list into 2 equal lists ------
separate :: [a] -> ([a],[a])
separate xs = splitAt (length xs `div` 2) xs
--- merge sort implementation ---------------
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort left)  (msort right)
          where (left, right) = separate xs



