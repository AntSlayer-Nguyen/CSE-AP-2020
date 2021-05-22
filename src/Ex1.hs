-- Double all elements in a list ---------------------------
doubleAll::[Int] -> [Int]
doubleAll x = map(\x->2*x) x

-- Search an element in a list and return the position -----
searchL :: Int-> [Int] -> Int
searchL x [] = error "Not found!!"
searchL x (a:l) = if(x==a) then 0 else (1 + (searchL x l))

-- Return a list of all positive numbers from a given list -- 
positives :: [Int] -> [Int]
positives [] = [] 
positives (a:l) = if((length l) == 0) then if (a <= 0) then [] else [a]  
                  else if (a <= 0) then (positives l) else ([a] ++ positives l)