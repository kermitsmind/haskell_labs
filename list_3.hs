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



-- Exercise 1. Use lambda calculus to implement the following functions 
-- a) a(x,y)=x+y;
-- b) constx(y) = x;
-- c) π2(x,y)=y;
-- d) id(x) = x
add = \x -> \y -> x + y
const = \x y -> x
piy = \x y -> y
idx = \x -> x



-- Exercise 2. Express map via foldr using lambda expressions.
map1 f xs = foldr (\x acc -> f x : acc) [] xs



-- Exercise 3. Let f :: b -> c and g :: a -> b. Express f.g (the composition) using lambda expressions.
-- f.g = \x -> f (g x)
-- map (\x -> negate (abs x)) [1,-2,3,-4]
-- map (negate . abs) [1,-2,3,-4]
-- f::a->b, g::b->c 
comp = \g f x -> g (f x)



-- Exercise 4. The expression let x=y in z is a ”syntactic sugar” for a certain expression in lambda calculus. Desugar it.
-- let x=y in z  is equal to let (\x -> y) in z = eta-red. lambdax.y
-- let x=y in z ~(lambdax -> z)y



-- Exercise 5. Let π1 =\x y->x and π2 =\x y->y. Calculate π1.π2 and π2.π1.
-- fg = (\f g x y -> f (g (x y))
-- pi1=(\x y -> x)
-- pi2=(\x y -> y)
-- pi1pi2 = pi1.pi2



-- Exercise 6. Come up with a lambda expression that cannot be β-reduced to a form that does not allow further β-reduction. 
-- (lambdax -> (x x)) = z
-- z z = (lambdax)-> (x x))z = beta-red. = z z
-- infinite loop
-- for example: (\x -> (x x)) (\y -> (y y))



-- Exercise 7. Let us define
--          data Point = Point Float Float
--          data Shape = Circle Point Float | Rectangle Point Point
-- where Circle x y models are planar circle with the center x and the radius y and Rectangle x y models a rectangle with top-left 
-- corner x and bottom-right corner y. Implement a function that calculates the surface of a given shape.
data Point = Point Float Float
data Shape = Circle Point Float | Rectangle Point Point

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x y) (Point x1 y1)) = abs(x1-x)*abs(y1-y)



-- Exercise 8. Let us define
--         data Vector3D a = Vector a a a
-- that models 3D vectors. Define addition, multiplication by a scalar and scalar multiplication for your vectors. 
-- Make it an instance of the class Show.
data Vector3 a = Vector3 a a a 

vadd :: (Num t) => Vector3 t -> Vector3 t -> Vector3 t
(Vector3 i j z) `vadd` (Vector3 k l m) = Vector3 (i+k) (j+l) (z+m)

vmult :: (Num t) => Vector3 t -> t -> Vector3 t
(Vector3 i j z) `vmult` m = Vector3 (i*m) (j*m) (z*m)

scallarmult :: (Num t) => Vector3 t -> Vector3 t -> t
(Vector3 i j z) `scallarmult` (Vector3 k l m) = i*k + j*l + z*m

instance Show a => Show (Vector3 a) where
  show (Vector3 x1 x2 x3) = "V = [" ++ show x1 ++ ", " ++ show x2 ++ ", " ++ show x3  ++ "]"



-- Exercise 9. Consider the following IntOrString type
--         data IntOrString = Word String | Number Int
-- Make it an instance of classes Eq and Show.
data IntOrString = Word String | Number Int deriving (Show, Eq)



-- Exercise 10. Let us recall the definition of a binary tree structure
--          data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
-- a) Make it an instance of the class Show so trees are printed nicely.
-- b) Make it an instance of the class Functor by implementing fmap for trees.
-- c) Make it an instance of the class Foldable by implementing foldr and foldl. They should ”start” from the 
--    right-most branch and the left-most branch respectively.
-- d) Implement functions that count the number of roots, count the number of leafs, determine whether a given x is 
--    an element of a tree, calculate the height of a tree. Fold may be useful here.
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) -- deriving (Show)

-- Functor
instance Functor Tree where
  fmap = treeMap

treeMap :: (a -> b) -> (Tree a) -> (Tree b)
treeMap _ Empty = Empty
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node left a right) = Node (treeMap f left) (f a) (treeMap f right) 

-- Foldable
instance Foldable Tree where
  foldr = treeFoldr
  foldl = treeFoldl

treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr f z Empty = z
treeFoldr f z (Leaf a) = f a z
treeFoldr f z (Node left a right) = treeFoldr f (f a (treeFoldr f z right)) left

treeFoldl :: (a -> b -> a) -> a -> Tree b -> a
treeFoldl f z Empty = z
treeFoldl f z (Leaf b) = f z b
treeFoldl f z (Node left b right) = treeFoldl f (f (treeFoldl f z left) b) right

-- is in tree -- idk what happend here...
-- IsInTree :: (Eq a) => a -> Tree a -> Bool
-- IsInTree x = treeFoldr (\y acc -> (y == x || acc)) False

-- counts
countNodes :: Tree a -> Int
countNodes tr = treeFoldr (+) 0 $ treeMap (\x -> 1) tr

countLeaves :: Tree a -> Int 
countLeaves (Leaf _) = 1
cpuntLeaves (Node left a right) = (countLeaves left) + (countLeaves right) 

-- height
treeHeightR :: Tree a -> Int 
treeHeightR Empty = 0
treeHeightR (Leaf a) = 1
treeHeightR (Node left _ right) = 1 + max (treeHeightR left) (treeHeightR right)



-- Exercise 11. Define trees with roots that may have any number of children (Hint: [Tree a]). Then
-- a) Make it a nice looking instance of the class Show.
-- b) Make it an instance of the class Functor, i.e. define fmap for your trees.
-- c) Make it a partial instance of the class Foldable, i.e. define some kind of fold. d) Repeat d) of the previous exercise.
data Rose_Tree a = Empty1 | Leaf1 a | Node1 a [Rose_Tree a] deriving (Show, Eq) -- Empty1, Leaf1, Node1 used because similar 
                                                                                -- names were already used in this file

-- Functor
instance Functor Rose_Tree where
  fmap _ Empty1 = Empty1
  fmap f (Leaf1 x) = Leaf1 (f x) 
  fmap f (Node1 x xs) = Node1 (f x) (map (fmap f) xs) 

-- Foldable
instance Foldable Rose_Tree where
  foldl _ z Empty1 = z
  foldl f z (Leaf1 x) = f z x
  foldl f z (Node1 x xs) = f temp_z x where
    temp_z = foldl (foldl f) z xs

  foldr _ z Empty1 = z 
  foldr f z (Leaf1 x) = f x z
  foldr f z (Node1 x xs) = f x temp_z where
    temp_z = foldr (\x y -> (foldr f y x)) z xs

-- Count
count_el :: Rose_Tree a -> Int
count_el t = foldl (\x y -> (x+1)) 0 t





-- Exercise 12 (*). Use the following tagged tree type
--         data Tagged tree a = Empty |
--         Leaf Integer a |
--         Node Integer (Tagged tree a) (Tagged tree a)
-- to transform lists to trees so that the k-th element of the list is the k-th leftmost leaf of the tree and you can 
-- access elements of the list in logarithmic time. The function list to tree :: [a] -> Tagged tree a should transform a 
-- given list into a balanced tree in O(n log(n)) time. The function get elem :: Integer -> Tagged tree a -> a should 
-- return the k-th element from the tagged tree in O(log(n)) time.



