-- Author : Charbel Aoun
-- USJ ID : 220599
 
-- Implementation of the LinkedList in Haskell

-- Functions :
-- -> display (Display the linked list)
-- -> addlast (Add element at the end of the linked list)
-- -> addfirst (Add element at the beginning of the linked list)
-- -> addatindex (Add element at a specific index in the linked list)
-- -> size (Get the size of the linked list)
-- -> sum' (Get the sum of the linked list)
-- -> removelast (Remove the last element of the linked list)
-- -> removefirst (Remove the first element of the linked list)
-- -> removeatindex (Remove element at a specific index in the linked list)
-- -> check (Check if an element exists in the linked list)
-- -> index (Get the index of an element in the linked list)
-- -> map' (Map a function to the linked list)
-- -> filterLL (Filter the linked list based on a predicate)
-- -> foldLeft (Left fold of the linked list)
-- -> foldRight (Right fold of the linked list)
-- -> reverseL (Reverse the linked list)
-- -> maxi (Get the maximum element in the linked list)
-- -> mini (Get the minimum element in the linked list)
-- -> mergeTwoSorted (Merge two sorted linked lists)
-- -> mergeSort' (Sort the linked list using merge sort)

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)

-- | Data type for a Linked List
--   It can either be an Empty list or a Node containing a value and a reference to the next node.
data LL a = Empty
          | Node {
              value :: a,   -- ^ The value stored in the node
              next  :: LL a -- ^ The next node in the linked list
          } deriving (Show)

-- | Display the linked list as a string.
--   It concatenates all elements separated by " -> ".
display :: (Show a) => LL a -> String
display list = display' list ""
    where
        -- | Helper function for display, recursively concatenates list values.
        display' Empty acc = acc -- Base case: If list is empty, return accumulated string
        display' (Node first next) acc
            | acc == ""  = display' next (show first) -- If accumulator is empty, add first value
            | otherwise  = display' next (acc ++ " -> " ++ show first) -- Append " -> " with the next node

-- | Add an element at the end of the linked list.
addlast :: LL a -> a -> LL a
addlast Empty newElement = Node newElement Empty -- Base case: If list is empty, return a new node
addlast (Node first next) newElement = Node first (addlast next newElement) -- Recursive case: go to the end of the list

-- | Add an element at the beginning of the linked list.
addfirst :: LL a -> a -> LL a
addfirst list newElement = Node newElement list -- Directly add a new node pointing to the current list

-- | Add an element at a specific index in the linked list.
addatindex :: LL a -> a -> Int -> LL a
addatindex list newElement index
    | index < 0 || index > s = error "Index out of range" -- Check for out-of-bound indices
    | index == 0 = addfirst list newElement -- Add at beginning if index is 0
    | index == s = addlast list newElement -- Add at end if index is the size of the list
    | otherwise = addatindex' list newElement index
    where
        s = size list -- Calculate the size of the list
        addatindex' (Node first next) newElement 1 = Node first (Node newElement next) -- Insert new element
        addatindex' (Node first next) newElement i = Node first (addatindex' next newElement (i - 1)) -- Recursive case

-- | Get the size of the linked list.
size :: LL a -> Int
size Empty = 0 -- Base case: If list is empty, its size is 0
size (Node first next) = 1 + size next -- Recursive case: Count 1 for the current node and call size on the next

-- | Get the sum of the elements in the linked list.
sum' :: (Num a) => LL a -> a
sum' list = sum1 list 0
    where
        -- | Helper function to calculate sum using an accumulator.
        sum1 Empty acc = acc -- Base case: If list is empty, return the accumulated sum
        sum1 (Node a next) acc = sum1 next (acc + a) -- Add the current node's value to the accumulator

-- | Remove the last element of the linked list.
removelast :: LL a -> LL a
removelast Empty = error "Empty list" -- Error if list is empty
removelast (Node first Empty) = Empty -- Base case: If there is only one node, return an empty list
removelast (Node first next) = Node first (removelast next) -- Recursive case: Continue until last node

-- | Remove the first element of the linked list.
removefirst :: LL a -> LL a
removefirst Empty = error "Empty list" -- Error if list is empty
removefirst (Node first next) = next -- Remove the first node by returning the next part of the list

-- | Remove an element at a specific index in the linked list.
removeatindex :: LL a -> Int -> LL a
removeatindex list index
    | index < 0 || index >= s = error "Index out of range" -- Check for valid indices
    | index == 0 = removefirst list -- Remove the first element if index is 0
    | index == s - 1 = removelast list -- Remove the last element if index is the last position
    | otherwise = removeatindex' list index
    where
        s = size list -- Calculate size of the list
        removeatindex' (Node first next) 0 = next -- Remove the node at index 0 in the sublist
        removeatindex' (Node first next) i = Node first (removeatindex' next (i - 1)) -- Recursive case


-- | Check if an element exists in the linked list.
check :: (Eq a) => LL a -> a -> Bool
check Empty _ = False -- Base case: If list is empty, return False
check (Node first next) element
    | first == element = True -- If the current node's value matches, return True
    | otherwise = check next element -- Otherwise, check the rest of the list

-- | Get the index of an element in the linked list.
index :: (Eq a) => LL a -> a -> Int
index list element = index' list element 0
    where
        index' Empty _ _ = -1 -- Base case: Return -1 if element is not found
        index' (Node first next) element i
            | first == element = i -- If the current node contains the element, return the index
            | otherwise = index' next element (i + 1) -- Recursive case

-- | Map a function to the linked list.
map' :: (a -> b) -> LL a -> LL b
map' _ Empty = Empty -- Base case: Return Empty if list is empty
map' f (Node first next) = Node (f first) (map' f next) -- Apply function f to each node's value

-- | Filter the linked list based on a predicate.
filterLL :: (a -> Bool) -> LL a -> LL a
filterLL _ Empty = Empty -- Base case: Return Empty if list is empty
filterLL p (Node x xs) 
  | p x       = Node x (filterLL p xs) -- Include the node if it satisfies the predicate
  | otherwise = filterLL p xs -- Otherwise, skip the node

-- | Left fold of the linked list.
foldLeft :: (b -> a -> b) -> b -> LL a -> b
foldLeft _ acc Empty = acc -- Base case: Return the accumulator if the list is empty
foldLeft f acc (Node first next) = foldLeft f (f acc first) next -- Update the accumulator with current value

-- | Right fold of the linked list.
foldRight :: (a -> b -> b) -> b -> LL a -> b
foldRight _ acc Empty = acc -- Base case: Return the accumulator if the list is empty
foldRight f acc (Node first next) = f first (foldRight f acc next) -- Call the function recursively

-- | Reverse the linked list.
reverseL :: LL a -> LL a
reverseL list = reverseL' list Empty
    where
        reverseL' Empty acc = acc -- Base case: If list is empty, return the accumulated list
        reverseL' (Node first next) acc = reverseL' next (Node first acc) -- Move node to the front of the new list

-- | Get the maximum element in the linked list.
maxi :: (Ord a) => LL a -> a
maxi Empty = error "Empty list" -- Error if list is empty
maxi (Node first next) = maxi' first next
    where
        maxi' currentMax Empty = currentMax -- Base case: Return the maximum value
        maxi' currentMax (Node first next)
            | first > currentMax = maxi' first next -- Update maximum if current node's value is larger
            | otherwise = maxi' currentMax next

-- | Get the minimum element in the linked list.
mini :: (Ord a) => LL a -> a
mini Empty = error "Empty list" -- Error if list is empty
mini (Node first next) = mini' first next
    where
        mini' currentMin Empty = currentMin -- Base case: Return the minimum value
        mini' currentMin (Node first next)
            | first < currentMin = mini' first next -- Update minimum if current node's value is smaller
            | otherwise = mini' currentMin next

-- | Merge two sorted linked lists.
--   Takes two sorted linked lists and merges them into a single sorted list.
mergeTwoSorted :: (Ord a) => LL a -> LL a -> LL a
mergeTwoSorted Empty list = list -- If the first list is empty, return the second list as it is already sorted.
mergeTwoSorted list Empty = list -- If the second list is empty, return the first list as it is already sorted.
mergeTwoSorted (Node first1 next1) (Node first2 next2)
    | first1 < first2 = Node first1 (mergeTwoSorted next1 (Node first2 next2)) 
      -- If the first node of the first list is smaller, keep it at the front 
      -- and recursively merge the rest of the first list with the second list.
    | otherwise = Node first2 (mergeTwoSorted (Node first1 next1) next2)
      -- If the first node of the second list is smaller or equal, keep it at the front 
      -- and recursively merge the first list with the rest of the second list.


-- | Sort the linked list using merge sort.
--   Sorts a linked list using the Merge Sort algorithm (divide and conquer).
mergeSort' :: (Ord a) => LL a -> LL a
mergeSort' Empty = Empty -- Base case: If the list is empty, it is already sorted, so return Empty.
mergeSort' (Node first Empty) = Node first Empty -- Base case: If the list has only one element, it is already sorted.
mergeSort' list = mergeTwoSorted (mergeSort' firstHalf) (mergeSort' secondHalf)
  -- Recursively sort the two halves (firstHalf and secondHalf) and merge them using mergeTwoSorted.
  where
    (firstHalf, secondHalf) = split list -- Split the list into two halves.
    split Empty = (Empty, Empty) -- If the list is empty, both halves are empty.
    split (Node first Empty) = (Node first Empty, Empty) 
      -- If the list has one element, assign it to the first half, and the second half is empty.
    split (Node first (Node second next)) = 
      (Node first (fst (split next)), Node second (snd (split next))) 
      -- Decompose the list into two parts:
      -- Assign the first element to the first half, the second element to the second half, 
      -- and recursively split the rest of the list into two halves.



-- TESTING EXECUTION TIME 
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
      putStrLn "=== Linked List Testing ==="

      -- Step 1: Create Linked List from the List
      putStrLn "Creating linked list from the list..."
      let list = foldr Node Empty numberList

      -- Step 2: Add Elements to the List
      startAdd <- getCurrentTimeInSeconds
      let listWithAdded = addlast list 50
      endAdd <- getCurrentTimeInSeconds

      -- Step 3: Remove Elements from the List
      startRemove <- getCurrentTimeInSeconds
      let listWithRemoved = removeatindex listWithAdded 0
      endRemove <- getCurrentTimeInSeconds

      -- Step 4: Count Elements in the List
      startCount <- getCurrentTimeInSeconds
      let nodeCount = size listWithRemoved
      putStrLn $ "Number of elements in the list: " ++ show nodeCount
      endCount <- getCurrentTimeInSeconds

      -- Step 5: Sum Elements in the List
      startSum <- getCurrentTimeInSeconds
      let totalSum = sum' listWithRemoved
      putStrLn $ "Sum of elements in the list: " ++ show totalSum
      endSum <- getCurrentTimeInSeconds

      -- End measuring total execution time
      endTotal <- getCurrentTimeInSeconds

      -- Display Execution Times
      putStrLn "\n=== Execution Times ==="
      putStrLn $ "Adding element: " ++ show (endAdd - startAdd) ++ " seconds"
      putStrLn $ "Removing element: " ++ show (endRemove - startRemove) ++ " seconds"
      putStrLn $ "Counting elements: " ++ show (endCount - startCount) ++ " seconds"
      putStrLn $ "Summing elements: " ++ show (endSum - startSum) ++ " seconds"
      putStrLn $ "\nTotal execution time: " ++ show (endTotal - startTotal) ++ " seconds"


--  TESTING THE FUNCTIONS
    -- Create the initial list
    -- let list = Node 1 (Node 2 (Node 3 (Node 4 (Node 5 Empty))))
    -- putStrLn $ "Original list: " ++ display list

    -- -- Add more elements
    -- let list1 = addfirst list 0
    -- putStrLn $ "After addFirst(0): " ++ display list1

    -- let list2 = addlast list1 6
    -- putStrLn $ "After addLast(6): " ++ display list2

    -- let list3 = addatindex list2 9 2
    -- putStrLn $ "After addAtIndex(9, 2): " ++ display list3

    -- -- Remove elements
    -- let list4 = removefirst list3
    -- putStrLn $ "After removeFirst: " ++ display list4

    -- let list5 = removelast list4
    -- putStrLn $ "After removeLast: " ++ display list5

    -- let list6 = removeatindex list5 2
    -- putStrLn $ "After removeAtIndex(2): " ++ display list6

    -- -- Calculate size and sum
    -- putStrLn $ "Size of list: " ++ display list6 ++ " = " ++ show (size list6)
    -- putStrLn $ "Sum of elements of the list: " ++ display list6 ++ " = " ++ show (sum' list6)

    -- -- Check for existence of elements
    -- putStrLn $ "Does number 5 exist in: " ++ display list6 ++ " ? -> " ++ if check list6 5 then "Yes" else "No"
    -- putStrLn $ "Does number 7 exist in: " ++ display list6 ++ " ? -> " ++ if check list6 7 then "Yes" else "No"

    -- -- Find indices of elements
    -- putStrLn $ "Index of 9 in the list: " ++ display list6 ++ " = " ++ show (index list6 9)
    -- putStrLn $ "Index of 7 in the list: " ++ display list6 ++ " = " ++ show (index list6 7)

    -- -- Map, filter
    -- let squaredList = map' (^2) list6
    -- putStrLn $ "Mapping square function on the list: " ++ display list6 ++ " Result: " ++ display squaredList

    -- let filteredList = filterLL even squaredList
    -- putStrLn $ "Filtering the list to keep even numbers from the list: " ++ display squaredList ++ " Result: " ++ display filteredList

    -- -- FoldLeft, FoldRight
    -- let sumLeft = foldLeft (+) 0 squaredList
    -- putStrLn $ "FoldLeft sum of the list: " ++ display squaredList ++ " = " ++ show sumLeft

    -- let productRight = foldRight (*) 1 squaredList
    -- putStrLn $ "FoldRight product of the list: " ++ display squaredList ++ " = " ++ show productRight

    -- -- Reverse the list
    -- let reversedList = reverseL squaredList
    -- putStrLn $ "Reversing the list: " ++ display squaredList ++ " Result: " ++ display reversedList

    -- -- Find maximum and minimum
    -- putStrLn $ "Maximum element of the list: " ++ display reversedList ++ " = " ++ show (maxi reversedList)
    -- putStrLn $ "Minimum element of the list: " ++ display reversedList ++ " = " ++ show (mini reversedList)

    -- -- MergeSort
    -- let sortedList = mergeSort' reversedList
    -- putStrLn $ "Sorting the list: " ++ display reversedList ++ " using MergeSort: " ++ display sortedList

  

    
