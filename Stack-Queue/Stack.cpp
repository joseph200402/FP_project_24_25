// Sophia Jarjoura 
// USJ ID : 220590

#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

// Define the Stack node
template <typename T>
struct Stack {
    const T value;
    const std::shared_ptr<const Stack> next;

    Stack(T val, std::shared_ptr<const Stack> nxt = nullptr) : value(val), next(nxt) {}
};

// Utility function to create a new stack node
template <typename T>
std::shared_ptr<const Stack<T>> createStackNode(T value, std::shared_ptr<const Stack<T>> next = nullptr) {
    return std::make_shared<const Stack<T>>(value, next);
}

// Create an empty stack
template <typename T>
std::shared_ptr<const Stack<T>> emptyStack() {
    return nullptr;
}

// Push an element onto the stack
template <typename T>
std::shared_ptr<const Stack<T>> push(T value, std::shared_ptr<const Stack<T>> stack) {
    return createStackNode(value, stack);
}

// Pop an element from the stack
template <typename T>
std::pair<T, std::shared_ptr<const Stack<T>>> pop(std::shared_ptr<const Stack<T>> stack) {
    if (!stack) throw std::out_of_range("Cannot pop from an empty stack");
    return {stack->value, stack->next};
}

// Peek at the top element without removing it
template <typename T>
T peek(std::shared_ptr<const Stack<T>> stack) {
    if (!stack) throw std::out_of_range("Stack is empty");
    return stack->value;
}

// Check if the stack is empty
template <typename T>
bool isEmpty(std::shared_ptr<const Stack<T>> stack) {
    return !stack;
}

// Get the size of the stack
template <typename T>
int size(std::shared_ptr<const Stack<T>> stack) {
    if (!stack) return 0;
    return 1 + size(stack->next);
}

// Display the stack
template <typename T>
std::string displayStack(const std::shared_ptr<const Stack<T>>& stack) {
    if (!stack) return "EmptyStack";
    return std::to_string(stack->value) + (stack->next ? " " + displayStack(stack->next) : "");
}

int main() {
    auto stack = emptyStack<int>();

    stack = push(10, stack);
    stack = push(20, stack);
    stack = push(30, stack);

    std::cout << "Stack after pushing 10, 20, 30: " << displayStack(stack) << "\n";
    std::cout << "Peek at top of stack: " << peek(stack) << "\n";

    auto [top, newStack] = pop(stack);
    std::cout << "After popping top: " << top << "\n";
    std::cout << "Stack now: " << displayStack(newStack) << "\n";

    std::cout << "Stack size: " << size(newStack) << "\n";

    return 0;
}