-- Sophia Jarjoura 
-- USJ ID : 220590


-- Define the Stack data type
data Stack a = EmptyStack          -- Represents an empty stack
             | StackNode a (Stack a) -- Represents an element and the rest of the stack
             deriving (Show, Eq)

-- Create an empty stack
emptyStack :: Stack a
emptyStack = EmptyStack

-- Push an element onto the stack
push :: a -> Stack a -> Stack a
push x stack = StackNode x stack -- Add the new element to the top of the stack

-- Pop an element from the stack
pop :: Stack a -> (a, Stack a)
pop EmptyStack = error "Cannot pop from an empty stack" -- Error for empty stack
pop (StackNode x rest) = (x, rest) -- Return the top element and the rest of the stack

-- Peek at the top element without removing it
peek :: Stack a -> a
peek EmptyStack = error "Stack is empty" -- Error for empty stack
peek (StackNode x _) = x -- Return the top element

-- Check if the stack is empty
isEmpty :: Stack a -> Bool
isEmpty EmptyStack = True
isEmpty _ = False

-- Get the size of the stack
size :: Stack a -> Int
size EmptyStack = 0
size (StackNode _ rest) = 1 + size rest