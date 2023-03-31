module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type, or 
  wrap a binary search tree.
  Extra marks will be awarded for efficient implementations if appropriate.

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
-- newtype Set a = Set { unSet :: [a] }
data Set a = Leaf | Node (Set a) a (Set a) deriving (Show, Read)

-- Balancing for the AVL tree inspired by https://www.cs.uleth.ca/~gaur/post/avl/
height :: (Ord a) => Set a -> Int
height Leaf = -1
height (Node l x r) = 1 + max (height l) (height r)

balanced :: (Ord a) => Set a -> Bool
balanced Leaf = True
balanced (Node l x r) | not (balanced l)              = False
                      | not (balanced r)              = False
                      | abs (height l - height r) > 1 = False
                      | otherwise                     = True

left :: Set a -> Set a
left Leaf = empty
left (Node l x r) = l

right :: Set a -> Set a
right Leaf = empty
right (Node l x r) = r

value :: (Ord a) => Set a -> a
value (Node l x r) = x

rotate :: (Ord a) => Set a -> Set a
rotate Leaf = empty  -- AVL implemention assumes a balanced pre-rotation tree
rotate (Node l x r) | not (balanced l) = Node (rotate l) x r
                    | not (balanced r) = Node l x (rotate r)
                    -- AVL balance-checking means that worst-case time complexity for insertions is in fact O(2log(n)).
                    -- BST insert might sometimes perform better than AVL owing to an overhead for small values of n
                    -- However in the average case the AVL tree and BST tree implementation are both basically O(log(n))
                    -- The advantage of AVL is that on ascending/descending lists the balancing reduces the height significantly
rotate t@(Node l x r)
                -- Single Rotation: Right Right
                | height l + 1     < height r &&
                  height (left r)  < height (right r)  =
                      Node (Node l x (left r)) (value r) (right r)
                -- Single Rotation: Left Left
                | height r + 1     < height l &&
                  height (right l) < height (left l)   =
                      Node (left l) (value l) (Node (right l) x r)
                -- Double Rotation: Right Left
                | height l + 1     < height r && 
                  height (left r)  > height (right r)  =
                     Node (Node l x (left (left r))) (value (left r)) (Node (right (left r)) (value r) (right r))
                -- Double Rotation: Left Right
                | height r + 1     < height l && 
                  height (right l) > height (left l)   =
                     Node (Node (left l) (value l) (left (right l))) (value (right l)) (Node (right (right l)) x r)
                | otherwise                            = t

example1 = fromList [1,2,3]
example2 = fromList [5,1,6,2,3,4,4]
example2' = toList example2
example3 = fromList [7,8,9,9,10]
example4 = fromList [2,4,10,11,12]
example5 = fromList [19,-1,144,8,2,1,4,3,7,0,-99,100,55]
{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList Leaf = []
toList (Node t1 v t2) = toList t1 ++ v : toList t2

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: (Ord a) => [a] -> Set a
fromList = foldr insert empty


{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- Merge sort and/or insertion sort might be used later
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = splitAt lhx xs
           where lhx = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort  xs = merge (msort left) (msort right)
            where (left,right) = halve xs

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)
   where insert' x [] = [x]
         insert' x (y:ys) |  x <= y = x : (y:ys)
                         | otherwise = y : insert' x ys

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
   s1 == s2 = toList s1 == toList s2
  

-- the empty set
empty :: Set a
empty = Leaf


-- Set with one element
singleton :: a -> Set a
singleton x = Node Leaf x Leaf

-- insert an element of type a into a Set
-- make sure there are no duplicates!
-- Insert for AVL implementation
insert :: (Ord a) => a -> Set a -> Set a
insert n Leaf = Node Leaf n Leaf
insert n t@(Node l x r)
    | x < n      = rotate (Node l x (insert n r))
    | x > n      = rotate (Node (insert n l) x r)
    | otherwise  = t

-- Insert for BST implementation
insert' :: (Ord a) => a -> Set a -> Set a
insert' x Leaf = Node Leaf x Leaf
insert' x t@(Node t1 v t2)
   | v < x  = Node t1 v (insert' x t2)
   | v > x  = Node (insert' x t1) v t2
   | v == x = t

-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union = setfoldr insert

-- union' :: (Ord a) => Set a -> Set a -> Set a
-- union' Leaf t = t
-- union' (Node t1 v t2) t = union' t2 (union' t1 (insert v t))

-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = setfoldr (\n acc -> if member n s2 then insert n acc else acc) s1 empty

-- intersection' :: (Ord a) => Set a -> Set a -> Set a
-- intersection' Leaf s2 = empty
-- intersection' (Node t1 v t2) s2
--   | member v s2 = insert v (union' (intersection' t1 s2) (intersection' t2 s2))
--   | otherwise   = union' (intersection' t1 s2) (intersection' t2 s2)


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = setfoldr (\n acc -> if not(member n s2) then insert n acc else acc) s1 empty

-- difference' :: (Ord a) => Set a -> Set a -> Set a
-- difference' Leaf s2 = empty
-- difference' (Node t1 v t2) s2
--   | not (member v s2) = insert v (union' (difference' t1 s2) (difference' t2 s2))
--   | otherwise         = union' (difference' t1 s2) (difference' t2 s2)

-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member _ Leaf = False
member x (Node t1 v t2)
  | x == v      = True
  | x < v       = member x t1
  | x > v       = member x t2


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality Leaf = 0
cardinality (Node t1 v t2) = cardinality t1 + 1 + cardinality t2


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f Leaf = empty
setmap f (Node t1 v t2) = Node (setmap f t1) (f v) (setmap f t2)


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f Leaf c = c
setfoldr f (Node t1 v t2) c = setfoldr f t2 (setfoldr f t1 (f v c))


fromList' :: [a] -> Set a
fromList' [] = empty
fromList' (x:xs) = Node (fromList' t1) x (fromList' t2)
  where (t1, t2) = splitAt (length xs `div` 2) xs

  
-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }

powerSet :: Set a -> Set (Set a)
powerSet a = fromList' [fromList' x | x <- powerSet' (toList a)]

listConvert :: Ord a => [Set a] -> [[a]]
listConvert xs = msort [msort $ toList x | x <- xs]

-- This function should help convert powerSet to list, it may facilitate testing
powerSetToList xs = listConvert (toList xs)

powerSet' :: [a] -> [[a]]
powerSet' xs = powerSet'' xs [[]]
  where
    powerSet'' [] acc = acc
    powerSet'' (x:xs) acc = powerSet'' xs (acc ++ [y ++ [x] | y <- acc])

-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian Leaf Leaf = empty
cartesian s1 s2 = fromList' (cartesian' (toList s1) (toList s2))

cartesian' :: [a] -> [b] -> [(a, b)]
cartesian' l1 l2 = [(a,b) | a <- l1, b <- l2]


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition _ Leaf = (empty, empty)
partition f (Node l x r)
  | f x       = (Node l1 x r1, combine l2 r2)
  | otherwise = (combine l1 r1, Node l2 x r2)
  where (l1, l2) = partition f l
        (r1, r2) = partition f r
        combine Leaf r' = r'
        combine (Node l x r) r' = Node l x (combine r r')

-- You can use this in order to then compare the output of partition with the corresponding list result
partitionToList :: (Set a, Set a) -> [[a]]
partitionToList (xs,ys) = [toList xs, toList ys]

{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

-}
