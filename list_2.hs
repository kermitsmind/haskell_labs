------------------------------------------------------------------
--                                                               |
-- To write script like this one and                             |
-- execute it in console, one need to                            |
-- 1. write a script SCRIPT_NAME.hs                              |
-- 2. open terminal in a folder containing the script            |
-- 3. run 'ghci' to open prelude >                               |
-- 4. run ':l SCRIPT_NAME.hs' to load previously defined         |
--    functions                                                  |
--                                                               | 
------------------------------------------------------------------



-- Exercise 1. Implement a function that for a list of integers calculates its sum of squares.
squareInt :: Int -> Int
squareInt x = x*x
-- 1st solution
squareList :: [Int] -> Int
squareList [] = 0
squareList xs = sum(map squareInt xs)
-- 2nd solution
sumSqInt :: [Int] -> Int
sumSqInt x = foldl (+) 0 (map (^2) x)



-- Exercise 2. Implement a function that for a list of integers calculates its sum of squares of prime members.
sumSqtIntPrime :: [Int] -> Int 
sumSqtIntPrime x = foldl (+) 0 (map (^2) (filter isPrime x))

isPrime :: Int -> Bool 
isPrime k = if k > 1 then length [ x|x <- [2..k-1 ], k `mod` x == 0] == 0 else False



-- Exercise 3. Implement a function that for a list of integers returns how many even members it contains.
evenCount :: [Int] -> Int
evenCount [] = 0
evenCount x = n
    where n = length xf
          xf = [ xf|xf <- x, xf `mod` 2 == 0]



-- Exercise 4. Implement a function that for a list of integers calculates the mean of its members. Try not to 
-- use explicitly the length of the list.
listlen xs = if null xs
             then 0
             else 1 + (listlen (tail xs))

sumx xs = if null xs
         then 0
         else (head xs) + sumx (tail xs)

mean xs = if null xs
          then 0
          else (fromIntegral (sumx xs)) / (fromIntegral (listlen xs)) 



-- Exercise 5. Implement foldr without checking it out in the documentation.



-- Exercise 6. Express map via foldr and foldl. Hint: it may be a good idea to use z=[].
myMapR f = foldr(\x xs -> f x : xs ) []
myMapL f = foldl(\xs x ->  xs ++ [(f x)] ) []


-- Exercise 7. Implement a function rev rev :: [[Char]] -> [[Char]] that takes a list of strings and returns the list 
-- of reversed strings in reversed order, i.e. rev rev ["lorem", "ipsum"] == ["muspi", "merol"]
-- oneStr :: [Char] -> [Char]
oneStr s = foldl (\x y -> y:x) [] s

-- rev_rev :: [[Char]] -> [[Char]]
rev_rev [] = []
rev_rev s = foldl (\x y -> y:x) [] (map oneStr s)



-- Exercise 8. Implement a function my filter :: a -> Bool -> [a] -> [a] that takes a predicate p :: a -> Bool, list of 
-- elements, and returns a list of elements satisfying p in two ways:
--     (i) using recursion without maps or folds; 
--     (ii) using maps or folds.
-- p :: a  -> Bool
-- p x | x > 0 = True
--    | otherwise = False
myFilter_1 :: (a -> Bool) -> [a] -> [a]
myFilter_1 _ [] = []
myFilter_1 p (x:xs) | p x = x : myFilter_1 p xs
                    | otherwise = myFilter_1 p xs
-- to run ine needs to type: myFilter_1 (p) [-1,2,3]

myFilter_2 :: (a -> Bool) -> [a] -> [a]
myFilter_2 _ [] = []
myFilter_2 p xs = [ x|x <- xs, p x ]



-- Exercise 9. Implement a function approx e :: Int -> Double calculating for each na- tural 􏰁n 1 for each natural n. 
-- It should work pretty fast, e.g. calculating k! from the k=0 k! ground with each ”iteration” is unacceptable. 
-- Hint: use accumulator storing k!
inv x = 1 / x

facHelper 0 acc = acc
facHelper n acc = facHelper (n-1) (acc*n)

factorial 0 = 1
factorial n = facHelper n 1

approx_e :: Int -> Double 
approx_e n
    | n == 0 = 1
    | n == 1 = 2
    | otherwise = 2 + sum (map (\n->1/(fromIntegral n)^2) [2..n])