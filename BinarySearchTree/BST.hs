-- Author : Joseph Doumit Bader Tarabay
-- USJ ID : 221360
-- Date: 02-12-2024

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Environment (getArgs)

-- Binary Search Tree

-- Functions:
-- 1. emptyTree: Create an empty tree
-- 2. singleTree: Create a tree with a single element
-- 3. addElement: Add an element to the BST

-- 4. fromList: Create a tree from a list of elements
-- 5. toList: Convert the tree to a sorted list (in-order traversal)

-- 6. fromSortedList: Balance a BST from a sorted list
-- 7. fromUnsortedList: Create a balanced BST from an unsorted list
-- 8. balance: Balance the BST

-- 9. sumTree: Calculate the sum of all elements in the tree
-- 10. productTree: Calculate the product of all elements in the tree

-- 11. minElement: Find the minimum element in the tree
-- 12. maxElement: Find the maximum element in the tree

-- 13. count: Count the total number of nodes in the tree
-- 14. depth: Calculate the depth of the tree

-- 15. contains: Check if an element exists in the tree

-- 16. removeMin: Remove the smallest element and return the updated tree
-- 17. removeMax: Remove the largest element and return the updated tree
-- 18. deleteElement: Delete an element from the tree

-- 19. inOrderList: In-order traversal
-- 20. preOrderList: Pre-order traversal
-- 21. postOrderList: Post-order traversal

-- 22. filterTree: Filter the BST and ensure the resulting tree is balanced
-- 23. mapTree: Map a function over the tree and ensure the resulting tree is balanced

-- 24. showTree: Custom Show instance for BST


-- _______________________________________Binary Search Tree____________________________________________________


-- Define the Binary Search Tree data type
-- Type constructor

data BST a = Empty
           | Node a (BST a) (BST a)
           deriving (Eq)

-- Create an empty tree
emptyTree :: BST a
emptyTree = Empty

-- Create a tree with a single element
singleTree :: a -> BST a
singleTree x = Node x Empty Empty

-- Add an element to the BST
addElement :: (Ord a) => a -> BST a -> BST a
addElement x Empty = Node x Empty Empty
addElement x (Node value left right)
  | x < value = Node value (addElement x left) right -- Add to left if smaller
  | otherwise = Node value left (addElement x right) -- Add to right if equal or greater

-- Create a tree from a list of elements
fromList :: (Ord a) => [a] -> BST a
fromList = foldr addElement emptyTree

-- Convert the tree to a sorted list (in-order traversal)
toList :: BST a -> [a]
toList Empty = []
toList (Node value left right) = toList left ++ [value] ++ toList right

-- Balance a BST from a sorted list
fromSortedList :: [a] -> BST a
fromSortedList [] = Empty
fromSortedList xs = Node mid (fromSortedList left) (fromSortedList right)
  where
    midIdx = length xs `div` 2
    mid = xs !! midIdx
    left = take midIdx xs
    right = drop (midIdx + 1) xs

-- Create a balanced BST from an unsorted list
fromUnsortedList :: (Ord a) => [a] -> BST a
fromUnsortedList = fromSortedList . quickSort
  where
    quickSort [] = []
    quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]

-- Balance the BST
balance :: BST a -> BST a
balance = fromSortedList . toList

-- Calculate the sum of all elements in the tree
sumTree :: (Num a) => BST a -> a
sumTree Empty = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

-- Calculate the product of all elements in the tree
productTree :: (Num a) => BST a -> a
productTree Empty = 1
productTree (Node value left right) = value * productTree left * productTree right

-- Find the minimum element in the tree
minElement :: (Ord a) => BST a -> a
minElement Empty = error "Tree is empty"
minElement (Node value Empty _) = value
minElement (Node _ left _) = minElement left

-- Find the maximum element in the tree
maxElement :: (Ord a) => BST a -> a
maxElement Empty = error "Tree is empty"
maxElement (Node value _ Empty) = value
maxElement (Node _ _ right) = maxElement right

-- Count the total number of nodes in the tree
count :: BST a -> Int
count Empty = 0
count (Node _ left right) = 1 + count left + count right

-- Calculate the depth of the tree
depth :: BST a -> Int
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- Check if an element exists in the tree
contains :: (Ord a) => a -> BST a -> Bool
contains _ Empty = False
contains x (Node value left right)
  | x == value = True
  | x < value  = contains x left
  | otherwise  = contains x right

-- Remove the smallest element and return the updated tree
removeMin :: BST a -> (a, BST a)
removeMin Empty = error "Tree is empty"
removeMin (Node value Empty right) = (value, right)
removeMin (Node value left right) = let (minValue, newLeft) = removeMin left
                                    in (minValue, Node value newLeft right)

-- Remove the largest element and return the updated tree
removeMax :: BST a -> (a, BST a)
removeMax Empty = error "Tree is empty"
removeMax (Node value left Empty) = (value, left)
removeMax (Node value left right) = let (maxValue, newRight) = removeMax right
                                    in (maxValue, Node value left newRight)

-- Delete an element from the tree
deleteElement :: (Ord a) => a -> BST a -> BST a
deleteElement _ Empty = Empty
deleteElement x (Node value left right)
  | x < value = Node value (deleteElement x left) right
  | x > value = Node value left (deleteElement x right)
  | otherwise = case right of
                  Empty -> left
                  _ -> let (minValue, newRight) = removeMin right
                       in Node minValue left newRight

-- In-order traversal
inOrderList :: BST a -> [a]
inOrderList = toList

-- Pre-order traversal
preOrderList :: BST a -> [a]
preOrderList Empty = []
preOrderList (Node value left right) = [value] ++ preOrderList left ++ preOrderList right

-- Post-order traversal
postOrderList :: BST a -> [a]
postOrderList Empty = []
postOrderList (Node value left right) = postOrderList left ++ postOrderList right ++ [value]

-- Filter the BST and ensure the resulting tree is balanced
filterTree :: (Ord a) => (a -> Bool) -> BST a -> BST a
filterTree _ Empty = Empty
filterTree f tree = fromSortedList $ filter f (toList tree)
  where
    -- Standard filter function applies the predicate to the list
    filter _ [] = []
    filter p (x:xs)
      | p x       = x : filter p xs
      | otherwise = filter p xs

-- Map a function over the tree and ensure the resulting tree is balanced
mapTree :: (Ord b) => (a -> b) -> BST a -> BST b
mapTree _ Empty = Empty
mapTree f tree = fromSortedList $ map f (toList tree)
  where
    -- Standard map function applies the function to every element in the list
    map _ [] = []
    map g (x:xs) = g x : map g xs

-- Custom Show instance for BST
instance (Show a) => Show (BST a) where
    show = showTree

-- Helper function to visualize the tree
showTree :: (Show a) => BST a -> String
showTree tree = unlines (visualizeTree tree 0)
  where
    visualizeTree :: (Show a) => BST a -> Int -> [String]
    visualizeTree Empty _ = ["--"] -- Represent empty nodes with "--"
    visualizeTree (Node value left right) depth =
        visualizeTree right (depth + 1) ++
        [replicate (depth * 4) ' ' ++ show value] ++
        visualizeTree left (depth + 1)


-- Function to measure time in seconds
getCurrentTimeInSeconds :: IO Double
getCurrentTimeInSeconds = realToFrac <$> getPOSIXTime

main :: IO ()
main = do
  -- Get command-line arguments to specify the input file
  args <- getArgs
  if null args
    then putStrLn "Please provide the path to the input text file."
    else do
      -- Start measuring total execution time
      startTotal <- getCurrentTimeInSeconds

      -- Read the file
      let fileName = head args
      content <- readFile fileName
      let numberList = map read (words content) :: [Int] -- Convert file content to a list of integers
      putStrLn "=== Binary Search Tree Testing ==="

      -- Step 1: Create Tree from the List
      putStrLn "Creating tree from the list..."
      let tree = fromList numberList

      -- Step 2: Add an Element to the Tree
      putStrLn "Adding element 50 to the tree..."
      let treeWithAdded = addElement 50 tree

      -- Step 3: Remove an Element from the Tree
      putStrLn "Removing element 50 from the tree..."
      let treeWithRemoved = deleteElement 50 treeWithAdded

      -- Step 4: Count Nodes in the Tree
      putStrLn "Counting nodes in the tree..."
      let nodeCount = count treeWithRemoved

      -- Step 5: Balance the Tree
      putStrLn "Balancing the tree..."
      let balancedTree = balance treeWithRemoved

      -- End measuring total execution time
      endTotal <- getCurrentTimeInSeconds

      -- Step 6: Display Final Results
      putStrLn "\n=== Final Results ==="
      putStrLn $ "Depth of Original Tree: " ++ show (depth tree)
      putStrLn $ "Depth of Balanced Tree: " ++ show (depth balancedTree)
      putStrLn $ "Number of Nodes: " ++ show nodeCount
      endTotal <- getCurrentTimeInSeconds
      putStrLn $ "\nTotal execution time: " ++ show (endTotal - startTotal) ++ " seconds"
