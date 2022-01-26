-- ----------------------------------------------------------------
--                                                               |
-- To write script like this one and                             |
-- execute it in console, one need to                            |
-- 1. write a script SCRIPT_NAME.hs                              |
-- 2. open terminal in a folder containing the script            |
-- 3. run 'ghci' to open prelude >                               |
-- 4. run ':l SCRIPT_NAME.hs' to load previously defined         |
--    functions                                                  |
--                                                               | 
-- ----------------------------------------------------------------



-- Exercise 1. Let f x = [x+1,x+2] and g x = [2*x,3*x]. Examine and calculate [1,4,7] >>= f and ([1,4,7] >>= f) >>= g.
move step pos
  | (step+pos) `elem` [-2..2] = Just (pos+step)
  | otherwise = Nothing
  
moves = move (1) (-1) >>= move 1 >>= move 1

move_list (x:xs) pos
  | length xs == 0 = Just (pos+x)
  | (x+pos) `elem` [-2..2] = move_list xs (pos+x)
  | otherwise = Nothing
  
  


-- Exercise 2. Implement a function that returns a list of all the possible outcomes of two (d6 and d20) dices roll. 
-- Use do notation or >>=.
allPairs xs ys =
   do
       x <- xs          
       y <- ys          
       return (x,y)     
-- [1..6] >>= (\x -> ([1..20] >>= (\y -> [(x,y)])))



-- Exercise 3. Desugar do x <- mx; f x. What should be the type of f? 



-- Exercise 4. Show that the following pieces of code
-- (i) do f <- mf; x <- mx; return (f x); 
-- (ii) do f <- mf; fmap f mx;
-- are equivalent.
-- do f <- mf; x <- mx; return (f x) == mf >>= (\f -> do x <- mx; return (f x)) == mf >>= (\f -> (mx >>= (\x -> return (fx)))) ==
-- mf >>= (\f -> (mx >>= return.f)) == mf >>= (\f -> (join(fmap return.f mx))) == mf >>= (\f -> (join((fmap return).(fmap f) mx))) ==
-- mf >>= (\f -> (join.(fmap return)((fmap f) mx))) == mf >>= (\f -> ((fmap f) mx))

-- do f <- mf; fmap f mx == mf >>= (\f -> fmap f mx)


-- Exercise 5. Explain how the do notation makes the list comprehension redundant.



-- Exercise 6. Can join be defined with return and >>= operator? Consider and simplify the following piece of code
--                                fun mmx = do
--                                  mx <- mmx
--                                  x <- mx
--                                  return x



-- Exercise 7. Identify >>=, >=>, and >> for monads you are familiar with (you should be familiar with at least two :-) ).



-- Exercise 8. Implement a model of ”walking a narrow path”. The wanderer starts at a position pos (an integer 
-- satisfying −3 < pos < 3) and moves forward and left or forward and right with each move (which changes the 
-- wanderer’s position by -1, 0, 1 respectively). If the wanderer wanders too much to one of the sides of the path, 
-- he dies (|pos| > 2). Implement
-- a) a function move :: Int -> Int -> Maybe Int that takes a move ∈ {−1, 0, 1} and a position and returns the new 
--    position (if the wanderer lives) or Nothing (if he dies). Use >>= to make a couple of moves. Examples of outcomes:
-- move 1 (-1) = Just 0, move 1 2 = Nothing
-- b) a function move list :: [Int] -> Int -> Maybe Int that does almost the same
--    thing, however it takes a list of moves instead of one, e.g.
--    move list [1,1,0,-1] 1 = Nothing, move list [1,0,-1,-1] 1 = Just 0. Use recursion and >>= or do notation.
move1 :: Int -> Int -> Maybe Int -- move1 because similar name is already in use
move1 step position
 | position < (-2) || position > 2 || a < (-2) || a > 2 = Nothing 
 | otherwise = Just a 
 where a = step+position

-- move 2 1
-- Nothing
-- move 1 ((move -1) 1) -- will not work because type does not match
-- (move (-1) 1) >>= (move 1) -- move1 1 expects Int and gives 1 Just 1 
-- (move (-1) 1) >>= (move 1) >>= (move 1)
-- Just 2
-- (move (-1) 1) >>= (move 1) >>= (move 1) >>= (move 1) Nothing
-- (move (-1) 1) >>= (move 1) >>= (move 1) >>= move (-1) Nothing -- no way back

move_list1 :: [Int] -> Int -> Maybe Int -- takes Int list, positions, return Int 
move_list1 [] pos = Just pos
move_list1 (x:xs) pos = (move x pos) >>= move_list1 xs -- we take from the forehead, 
                                                       -- we take position=move1 step position; argument binded 
                                                       -- we think like about position



-- Exercise 9. Implement a function that takes a starting position of a knight on a chess board of size n × k and returns 
-- a list of its possible positions in
-- a) 3 moves,
-- b) any number of moves, i.e. the number of moves is the function’s argument. Use >>= or do notation.
-- move_knight :: (Int,Int) -> (Int,Int) -> [(Int,Int)] 
-- move_knight (n,k) (x,y) = remove_duplicates $ leave_on_board (n,k) $
move_knight :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
move_knight (n,k) (x,y) = [(x+i,y+j) | i <- [-1,1], j <- [-2,2]] ++ [(x+j,y+i) | i <- [-1,1], j <- [-2,2]]
leave_on_board (n,k) xs = lob (n,k) xs [] where
    lob _ [] acc = acc
    lob (n,k) ((x,y):xs) acc = if (x<1 || x>n || y<1 || y>k) then (lob (n,k) xs acc) else (lob (n,k)) xs ((x,y):acc)
-- move_knight (8,8) (3,3)
-- (move_knight (8,8) (3,3)) >>= move_knight (8,8)