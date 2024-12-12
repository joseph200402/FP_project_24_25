-- Sophia Jarjoura 
-- USJ ID : 220590

-- Implementing a queue data structure

-- Define the Queue data type
data Queue a = EmptyQueue          -- Represents an empty queue
             | QueueNode a (Queue a) -- Represents an element and the rest of the queue
             deriving (Show, Eq)

-- Create an empty queue
emptyQueue :: Queue a
emptyQueue = EmptyQueue

-- Add an element to the back of the queue (enqueue)
enqueue :: a -> Queue a -> Queue a
enqueue x EmptyQueue = QueueNode x EmptyQueue 
enqueue x (QueueNode y rest) = QueueNode y (enqueue x rest)

-- Remove an element from the front of the queue (dequeue)
dequeue :: Queue a -> (a, Queue a)
dequeue EmptyQueue = error "Cannot dequeue from an empty queue" -- Error for empty queue
dequeue (QueueNode x rest) = (x, rest) -- Remove the first element

-- Peek at the front element without removing it
peek :: Queue a -> a
peek EmptyQueue = error "Queue is empty" -- Error for empty queue
peek (QueueNode x _) = x -- Return the first element

-- Check if the queue is empty
isEmpty :: Queue a -> Bool
isEmpty EmptyQueue = True
isEmpty _ = False

-- Get the size of the queue
size :: Queue a -> Int
size EmptyQueue = 0
size (QueueNode _ rest) = 1 + size rest