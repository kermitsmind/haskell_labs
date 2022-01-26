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



-- Exercise 1. Upgrade the following implementation of the factorial so that it is tail re- cursive and fast
--                    factorial 0 = 1
--                    factorial n = n * (factorial (n-1))
factorial :: Int -> Int
fachelper :: Int -> Int -> Int
fachelper 0 acc = acc
fachelper n acc = fachelper (n-1) (acc*n)
factorial n = fachelper n 1



-- Exercise 2. Upgrade the following implementation of list reversing so that it is tail recursive and fast
--                        rev [] = []
--                        rev (x:xs) = (rev xs) ++ [x]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

tailRecursiveRev [] = []
tailRecursiveRev (x : xs) = rev' xs [x]
 where
  rev' [] acc = acc
  rev' (x:xs) acc = rev' xs (x : acc)  




-- Exercise 3. Implement a function that for a given natural n quickly counts the amount of zeros at the end of n!
trailingZeros n
  | divided == 0 = 0
  | otherwise = divided + trailingZeros divided
  where
    divided = n `div` 5



-- Exercise 4. Implement your own functions that curry and de-curry functions, i.e. for f∈C(A×B) andg∈(CB)A
-- (my curry f) a b = f (a,b), (my decurry g) (a,b) = g a b.
add_f (x,y) = x+y
add_g x y = x+y

decurry (x,y) = add_g (fst (x,y)) (snd (x,y))
cuury x y = add_f (x,y)

(my_curry add_f) x y = add_f (x,y)
(my_decurry add_g) (a,b) = add_g a b



-- Exercise 5. Implement the sieve of Eratosthenes. Your solution should be fast.
primes n = sieve [2..n]
 where
  sieve (p:xs) 
   | p*p > n = p:xs
   | otherwise = p:sieve [n | n <- xs, n `mod` p /= 0]



-- Exercise 6. The Euler’s totient function φ : N+ → N is defined as follows
--             φ(n)=|{k∈N+ :k􏰀n&gcd(k,n)=1}|. 
--     Implement   
--     (a) the Euler’s totient function.
--     (b) a function f(n) = Ed∈{k∈N+:k|n} φ(d). (*) Put forward a hypothesis and try to prove it.
totient m = length([p | p <- [1..m], (gcd p m) == 1])



-- Exercise 7. 
    -- (a) Implement a function that calculates the n-th member of Fibonacci se- quence in a linear time.
    -- (b) The same for the sequence
    --     a0 = 1,
    --     a1 = 1,
    --     an = n + an−1 + an−2.
fib 0 x y = x+y
fib i x y = fib (i-1) y (x+y)
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fib (n-2) 0 1

-- main :: IO ()
-- main =  do
-- print(fibonacci 4)



-- Exercise 8. See the documentation of the function zipWith. Create an infinite list list of 
-- Fibonacci numbers using zipWith and lazy evaluation.



-- Exercise 9. Implement a function
--     (a) ecd that for a given string (a list of chars) eliminates consecutive duplicates, i.e.
--         ecd [1,1,2,3,3] == [1,2,3].
--     (b) encode that for a given string encodes consecutive duplicates with an integer, i.e.
--         encode [a,a,a,b,b,a,a] == [(a,3), (b,2), (a,2)]. 
--     (c) decode that decodes the previous one, i.e.
--         decode (encode xs) == xs
ecd [] = []
ecd (x:xs) = copy x xs
 where 
  copy x [] = [x]
  copy x (y:ys)
   | x == y = copy x ys
   | otherwise = x:(copy y ys)

encode :: (Eq a, Num b) => [a] -> [(a, b)]
encode [] = []
encode (x:xs) = pom 1 x xs
 where
  pom n x [] = [(x,n)]
  pom n x (y:ys)
   | x == y = pom (n+1) x ys
   | otherwise = (x,n):(pom 1 y ys)

decode [] = []
decode (x:xs) = pom1 x xs []
 where 
  pom1 x [] acc =  acc++(replicate (snd x) (fst x)) 
  pom1 x (y:ys) acc = pom1 y ys (acc++(replicate (snd x) (fst x)))



-- Exercise 10. Implement a function rev rev that for a list of string returns the reversed
-- list of reversed strings, i.e.
-- rev rev ["abc","xyz"] == ["zyx","cba"].
-- oneStr :: [Char] -> [Char]
oneStr s = foldl (\x y -> y:x) [] s

-- rev_rev :: [[Char]] -> [[Char]]
rev_rev [] = []
rev_rev s = foldl (\x y -> y:x) [] (map oneStr s)



-- Exercise 11. Implement a function substrings that for a given string returns the list
-- of all its substrings, i.e. substrings "abc" == ["a","b","c","ab","bc","abc"].
substrings x = [drop b (take a x) | a <- [1..length x], b <- [0..a-1]] 



-- Exercise 12. Implement a function power list that for a given list returns the list of all its sublists, i.e. 
-- power list [1,2,3] == [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]. 
power_list [] = [[]]
power_list (x:xs) = [x:sublist | sublist <- power_list xs] ++ power_list xs



-- Exercise 13. Implement a function perm that for a given list returns the list of all its
-- permutations, i.e. perm [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]. You may assume that the 
-- list is without duplicates.
perm1 :: [a] -> [[a]]
perm1 [] = [[]]
perm1 (x:xs) =
  foldr (++) [] (map (interleave [] x) (perm1 xs))
    where
      interleave :: [a] -> a -> [a] -> [[a]]
      interleave xs x [] = [xs ++ [x]]
      interleave xs x (y:ys) =
        (xs ++ (x:y:ys)) : 
          (interleave (xs ++ [y]) x ys)