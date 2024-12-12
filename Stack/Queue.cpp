//  Sophia Jarjoura 
//  USJ ID : 220590

#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

// Define the Queue node
template <typename T>
struct Queue {
    const T value;
    const std::shared_ptr<const Queue> next;

    Queue(T val, std::shared_ptr<const Queue> nxt = nullptr) : value(val), next(nxt) {}
};

// Utility function to create a new queue node
template <typename T>
std::shared_ptr<const Queue<T>> createQueueNode(T value, std::shared_ptr<const Queue<T>> next = nullptr) {
    return std::make_shared<const Queue<T>>(value, next);
}

// Create an empty queue
template <typename T>
std::shared_ptr<const Queue<T>> emptyQueue() {
    return nullptr;
}

// Add an element to the back of the queue (enqueue)
template <typename T>
std::shared_ptr<const Queue<T>> enqueue(std::shared_ptr<const Queue<T>> queue, T value) {
    if (!queue) return createQueueNode(value);
    return createQueueNode(queue->value, enqueue(queue->next, value));
}

// Remove an element from the front of the queue (dequeue)
template <typename T>
std::pair<T, std::shared_ptr<const Queue<T>>> dequeue(std::shared_ptr<const Queue<T>> queue) {
    if (!queue) throw std::out_of_range("Cannot dequeue from an empty queue");
    return {queue->value, queue->next};
}

// Peek at the front of the queue
template <typename T>
T peek(std::shared_ptr<const Queue<T>> queue) {
    if (!queue) throw std::out_of_range("Queue is empty");
    return queue->value;
}

// Check if the queue is empty
template <typename T>
bool isEmpty(std::shared_ptr<const Queue<T>> queue) {
    return !queue;
}

// Get the size of the queue
template <typename T>
int size(std::shared_ptr<const Queue<T>> queue) {
    if (!queue) return 0;
    return 1 + size(queue->next);
}

// Display the queue
template <typename T>
std::string displayQueue(const std::shared_ptr<const Queue<T>>& queue) {
    if (!queue) return "EmptyQueue";
    return std::to_string(queue->value) + (queue->next ? " " + displayQueue(queue->next) : "");
}

int main() {
    auto queue = emptyQueue<int>();

    queue = enqueue(queue, 10);
    queue = enqueue(queue, 20);
    queue = enqueue(queue, 30);

    std::cout << "Queue after enqueueing 10, 20, 30: " << displayQueue(queue) << "\n";
    std::cout << "Peek at front of queue: " << peek(queue) << "\n";

    auto [front, newQueue] = dequeue(queue);
    std::cout << "After dequeuing front: " << front << "\n";
    std::cout << "Queue now: " << displayQueue(newQueue) << "\n";

    std::cout << "Queue size: " << size(newQueue) << "\n";

    return 0;
}
