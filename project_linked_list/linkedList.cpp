// Author : Charbel Aoun
// USJ ID : 220599

// !!! THIS IS NOT THE IMPLEMENTATION OF THE LINKED LIST BASED ON FUNCTIONAL PROGRAMMING, 
//     THIS FILE IS JUST USED TO COMPARE THE PERFORMANCE AND EX TIME WITH THE LINKED LIST WRITTEN IN HASKELL !!!
//     YOU CAN FIND THE IMPLEMENTATION OF THE LINKED LIST BASED ON FUNCTIONAL PROGRAMMING IN THE FILE linked_list.cpp


#include <iostream>
#include <fstream>
#include <vector>
#include <chrono>
#include <stdexcept>

// Node structure for the linked list
class Node {
public:
    int value;
    Node* next;

    Node(int val) : value(val), next(nullptr) {}
};

// LinkedList class
class LinkedList {
private:
    Node* head;

public:
    // Constructor
    LinkedList() : head(nullptr) {}

    // Destructor to free memory
    ~LinkedList() {
        clear();
    }

    // Add a node to the beginning of the list
    void addFirst(int newElement) {
        Node* newNode = new Node(newElement);
        newNode->next = head;
        head = newNode;
    }

    // Add a node to the end of the list
    void addLast(int newElement) {
        Node* newNode = new Node(newElement);
        if (!head) {
            head = newNode;
            return;
        }
        Node* temp = head;
        while (temp->next) {
            temp = temp->next;
        }
        temp->next = newNode;
    }

    // Add a node at a specific index
    void addAtIndex(int index, int newElement) {
        if (index < 0) throw std::out_of_range("Index must be non-negative");
        if (index == 0) {
            addFirst(newElement);
            return;
        }
        Node* temp = head;
        for (int i = 0; i < index - 1; ++i) {
            if (!temp) throw std::out_of_range("Index exceeds list size");
            temp = temp->next;
        }
        Node* newNode = new Node(newElement);
        newNode->next = temp->next;
        temp->next = newNode;
    }

    // Remove the first node
    void removeFirst() {
        if (!head) throw std::out_of_range("Cannot remove from an empty list");
        Node* temp = head;
        head = head->next;
        delete temp;
    }

    // Remove the last node
    void removeLast() {
        if (!head) throw std::out_of_range("Cannot remove from an empty list");
        if (!head->next) {
            delete head;
            head = nullptr;
            return;
        }
        Node* temp = head;
        while (temp->next->next) {
            temp = temp->next;
        }
        delete temp->next;
        temp->next = nullptr;
    }

    // Remove a node at a specific index
    void removeAtIndex(int index) {
        if (index < 0) throw std::out_of_range("Index must be non-negative");
        if (index == 0) {
            removeFirst();
            return;
        }
        Node* temp = head;
        for (int i = 0; i < index - 1; ++i) {
            if (!temp->next) throw std::out_of_range("Index exceeds list size");
            temp = temp->next;
        }
        Node* toDelete = temp->next;
        if (toDelete) {
            temp->next = toDelete->next;
            delete toDelete;
        }
    }

    // Get the size of the list
    int size() const {
        int count = 0;
        Node* temp = head;
        while (temp) {
            count++;
            temp = temp->next;
        }
        return count;
    }

    // Get the sum of all elements in the list
    int sum() const {
        int total = 0;
        Node* temp = head;
        while (temp) {
            total += temp->value;
            temp = temp->next;
        }
        return total;
    }

    // Check if an element exists in the list
    bool check(int element) const {
        Node* temp = head;
        while (temp) {
            if (temp->value == element) return true;
            temp = temp->next;
        }
        return false;
    }

    // Get the index of an element in the list (-1 if not found)
    int index(int element) const {
        Node* temp = head;
        int i = 0;
        while (temp) {
            if (temp->value == element) return i;
            temp = temp->next;
            i++;
        }
        return -1;
    }

    // Print the list (for debugging)
    void display() const {
        Node* temp = head;
        while (temp) {
            std::cout << temp->value;
            if (temp->next) std::cout << " -> ";
            temp = temp->next;
        }
        std::cout << std::endl;
    }

    // Clear the entire list
    void clear() {
        while (head) {
            removeFirst();
        }
    }
};

double getCurrentTimeInSeconds() {
    return std::chrono::duration<double>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
}



// TESTING EXECUTION TIME 
// int main(int argc, char* argv[]) {
//     if (argc < 2) {
//         std::cerr << "Please provide the path to the input text file." << std::endl;
//         return 1;
//     }

//     try {
//         // Start measuring total execution time
//         double startTotal = getCurrentTimeInSeconds();

//         // Read the file
//         std::string fileName = argv[1];
//         std::ifstream file(fileName);
//         if (!file) {
//             std::cerr << "Could not open file: " << fileName << std::endl;
//             return 1;
//         }

//         std::vector<int> numberList;
//         int number;
//         while (file >> number) {
//             numberList.push_back(number);
//         }
//         file.close();

//         std::cout << "=== Linked List Testing ===" << std::endl;

//         // Step 1: Create Linked List from the List
//         std::cout << "Creating linked list from the list..." << std::endl;
//         LinkedList list;
//         for (int num : numberList) {
//             list.addLast(num);
//         }

//         // Step 2: Add Elements to the List
//         double startAdd = getCurrentTimeInSeconds();
//         list.addLast(50);
//         double endAdd = getCurrentTimeInSeconds();

//         // Step 3: Remove Elements from the List
//         double startRemove = getCurrentTimeInSeconds();
//         list.removeAtIndex(0);
//         double endRemove = getCurrentTimeInSeconds();

//         // Step 4: Count Elements in the List
//         double startCount = getCurrentTimeInSeconds();
//         int nodeCount = list.size();
//         std::cout << "Number of elements in the list: " << nodeCount << std::endl;
//         double endCount = getCurrentTimeInSeconds();

//         // Step 5: Sum Elements in the List
//         double startSum = getCurrentTimeInSeconds();
//         int totalSum = list.sum();
//         std::cout << "Sum of elements in the list: " << totalSum << std::endl;
//         double endSum = getCurrentTimeInSeconds();

//         // End measuring total execution time
//         double endTotal = getCurrentTimeInSeconds();

//         // Display Execution Times
//         std::cout << "\n=== Execution Times ===" << std::endl;
//         std::cout << "Adding element: " << (endAdd - startAdd) << " seconds" << std::endl;
//         std::cout << "Removing element: " << (endRemove - startRemove) << " seconds" << std::endl;
//         std::cout << "Counting elements: " << (endCount - startCount) << " seconds" << std::endl;
//         std::cout << "Summing elements: " << (endSum - startSum) << " seconds" << std::endl;
//         std::cout << "\nTotal execution time: " << (endTotal - startTotal) << " seconds" << std::endl;

//     } catch (const std::exception& ex) {
//         std::cerr << "Error: " << ex.what() << std::endl;
//         return 1;
//     }

//     return 0;
// }


// TESTING THE FUNCTIONS
// int main() {
//     // Create a LinkedList instance
//     LinkedList list;

//     // Test: Add elements to the list
//     std::cout << "Adding elements to the list..." << std::endl;
//     list.addFirst(10);   // Add to the beginning
//     list.addLast(20);    // Add to the end
//     list.addLast(30);    // Add to the end
//     list.addAtIndex(1, 15); // Add at index 1
//     std::cout << "List after additions: ";
//     list.display();

//     // Test: Remove elements from the list
//     std::cout << "\nRemoving elements from the list..." << std::endl;
//     list.removeFirst();    // Remove the first element
//     list.removeLast();     // Remove the last element
//     list.removeAtIndex(0); // Remove element at index 0
//     std::cout << "List after removals: ";
//     list.display();

//     // Test: Size and sum of the list
//     std::cout << "\nCalculating size and sum of the list..." << std::endl;
//     int size = list.size();
//     int totalSum = list.sum();
//     std::cout << "Size of the list: " << size << std::endl;
//     std::cout << "Sum of the elements in the list: " << totalSum << std::endl;

//     // Test: Check if elements exist
//     std::cout << "\nChecking for element existence..." << std::endl;
//     int checkElement = 20;
//     bool exists = list.check(checkElement);
//     std::cout << "Does " << checkElement << " exist in the list? " << (exists ? "Yes" : "No") << std::endl;

//     // Test: Get index of an element
//     std::cout << "\nGetting index of elements..." << std::endl;
//     int elementIndex = list.index(15);
//     if (elementIndex != -1)
//         std::cout << "Index of 15: " << elementIndex << std::endl;
//     else
//         std::cout << "15 not found in the list." << std::endl;

//     // Test: Display final state of the list
//     std::cout << "\nFinal state of the list: ";
//     list.display();

//     // Test: Clear the list
//     std::cout << "\nClearing the list..." << std::endl;
//     list.clear();
//     std::cout << "List after clearing: ";
//     list.display();

//     return 0;
// }