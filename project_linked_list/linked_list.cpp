// Author : Charbel Aoun
// USJ ID : 220599
 
// Implementation of the LinkedList in C++ functional programming
 
// Functions :
//      createNode (Rather than repeatedly using std::make_shared throughout the program, the createNode function is used when we need to create new LL nodes)
//      display (prints the elements of the linked list)
//      addFirst (adds an element to the beginning of the list)
//      addLast (adds an element to the end of the list)
//      addAtIndex (adds an element at a specific index)
//      removeFirst (removes the first element)
//      removeLast (removes the last element)
//      removeAtIndex (removes an element at a specific index)
//      size (returns the size of the list)
//      sum (returns the sum of the elements in the list)
//      check (checks if an element exists in the list or not)
//      index (returns the index of an element in the list if it exists)
//      map (applies a function to each element in the list)
//      filterLL (filters the list based on a predicate)
//      foldLeft (folds the list from left to right)
//      foldRight (folds the list from right to left)
//      reverseL (reverses the list)
//      maxi (returns the maximum element in the list)
//      mini (returns the minimum element in the list)
//      mergeTwoSorted (merges two sorted lists)
//      mergeSort (sorts the list using merge sort)

#include <iostream>
#include <memory>
#include <string>
#include <functional>
#include <chrono>
#include <vector>
#include <fstream>
#include <tuple>
// Define a linked list node

template <typename T>
struct LL {
    const T value;
    const std::shared_ptr<const LL> next;

    LL(T val, std::shared_ptr<const LL> nxt = nullptr) : value(val), next(nxt) {}
};

//createNode (Rather than repeatedly using std::make_shared throughout the program, the createNode function is used when we need to create new LL nodes)
template <typename T>
std::shared_ptr<const LL<T>> createNode(T value, std::shared_ptr<const LL<T>> next = nullptr) {
    return std::make_shared<const LL<T>>(value, next);
}

//display (prints the elements of the linked list)
template <typename T>
std::string display(const std::shared_ptr<const LL<T>>& list) {
    if (!list) return ""; // Base case: empty list
    if (!list->next) 
        return std::to_string(list->value); // Base case: single-node list (no arrow)
    else 
        return std::to_string(list->value) + "->" + display(list->next); // Recursive case: add "->" between nodes
}



//addFirst (adds an element to the beginning of the list)
template <typename T>
std::shared_ptr<const LL<T>> addFirst(const std::shared_ptr<const LL<T>>& list, T newElement) { 
    return createNode(newElement, list); // Create a new node with the new element and the current list
}

//addLast (adds an element to the end of the list)
template <typename T>
std::shared_ptr<const LL<T>> addLast(const std::shared_ptr<const LL<T>>& list, T newElement) {
    if (!list) return createNode(newElement);
    return createNode(list->value, addLast(list->next, newElement)); // Recurse to the end of the list
}

//addAtIndex (adds an element at a specific index)
template <typename T>
std::shared_ptr<const LL<T>> addAtIndex(const std::shared_ptr<const LL<T>>& list, T newElement, int index) {
    if (index < 0) throw std::out_of_range("Index must be non-negative"); // Check for negative index
    if (!list && index > 0) throw std::out_of_range("Index exceeds list size"); // Check for index out of bounds
    if (index == 0) return createNode(newElement, list); // Base case: insert at the current position
    return createNode(list->value, addAtIndex(list->next, newElement, index - 1)); // Recurse to the desired index
}

//removeFirst (removes the first element)
template <typename T>
std::shared_ptr<const LL<T>> removeFirst(const std::shared_ptr<const LL<T>>& list) {
    if (!list) throw std::out_of_range("Cannot remove from an empty list"); // Check for empty list
    return list->next; // Return the list without the first element
}

//removeLast (removes the last element)
template <typename T>
std::shared_ptr<const LL<T>> removeLast(const std::shared_ptr<const LL<T>>& list) {
    if (!list) throw std::out_of_range("Cannot remove from an empty list"); // Check for empty list
    if (!list->next) return nullptr; // Base case: remove the last element
    return createNode(list->value, removeLast(list->next)); // Recurse to the end of the list
}

//removeAtIndex (removes an element at a specific index)
template <typename T>
std::shared_ptr<const LL<T>> removeAtIndex(const std::shared_ptr<const LL<T>>& list, int index) {
    if (index < 0) throw std::out_of_range("Index must be non-negative"); // Check for negative index
    if (!list) throw std::out_of_range("Index exceeds list size"); // Check for index out of bounds
    if (index == 0) return list->next; // Base case: remove the element at the current index
    return createNode(list->value, removeAtIndex(list->next, index - 1)); // Recurse to the desired index
}


//size (returns the size of the list)
template <typename T>
int size(const std::shared_ptr<const LL<T>>& list) {
    if (!list) return 0; // Base case: empty list
    return 1 + size(list->next); // Recursive case: count the current node and proceed
}

//sum (returns the sum of the elements in the list)
template <typename T>
T sum(const std::shared_ptr<const LL<T>>& list, T acc = 0) {
    if (!list) return acc; // Base case: empty list, return accumulated sum
    return sum(list->next, acc + list->value); // Add current value to accumulator and recurse
}

//check (checks if an element exists in the list or not)
template <typename T>
bool check(const std::shared_ptr<const LL<T>>& list, T element) {
    if (!list) return false; // Base case: empty list, element not found
    return list->value == element || check(list->next, element); // Check current node or recurse
}

//index (returns the index of an element in the list if it exists)
template <typename T>
int index(const std::shared_ptr<const LL<T>>& list, T element, int i = 0) {
    if (!list) return -1; // Base case: element not found
    if (list->value == element) return i; // Base case: element found
    return index(list->next, element, i + 1); // Recursive case: increment index and search next
}

//map (applies a function to each element in the list)
template <typename T, typename U>
std::shared_ptr<const LL<U>> map(const std::shared_ptr<const LL<T>>& list, std::function<U(T)> f) {
    if (!list) return nullptr;
    return createNode(f(list->value), map(list->next, f));
}

//filterLL (filters the list based on a predicate)
template <typename T>
std::shared_ptr<const LL<T>> filterLL(const std::shared_ptr<const LL<T>>& list, std::function<bool(T)> predicate) {
    // Base case: if the list is empty, return nullptr
    if (!list) return nullptr;

    // Recursive case: apply the predicate to the current node's value
    if (predicate(list->value)) {
        return createNode(list->value, filterLL(list->next, predicate)); // Keep the current node
    } else {
        return filterLL(list->next, predicate); // Skip the current node
    }
}

//foldLeft (folds the list from left to right)
template <typename A, typename B>
B foldLeft(std::function<B(B, A)> f, B acc, const std::shared_ptr<const LL<A>>& list) {
    if (!list) return acc; // Base case: return the accumulated value when the list is empty
    return foldLeft(f, f(acc, list->value), list->next); // Recursive case: apply the function and continue
}

//foldRight (folds the list from right to left)
template <typename A, typename B>
B foldRight(std::function<B(A, B)> f, B acc, const std::shared_ptr<const LL<A>>& list) {
    if (!list) return acc; // Base case: return the accumulated value when the list is empty
    return f(list->value, foldRight(f, acc, list->next)); // Recursive case: apply the function to the head and the result of the recursive call
}

//reverseLHelper (helper function that uses an accumulator to reverse the list)
template <typename T>
std::shared_ptr<const LL<T>> reverseLHelper(const std::shared_ptr<const LL<T>>& list,
                                            const std::shared_ptr<const LL<T>>& acc) {
    if (!list) return acc; // Base case: when the input list is empty, return the accumulated list
    // Recursive case: prepend the current value to the accumulator
    return reverseLHelper(list->next, createNode(list->value, acc));
}

//reverseL (reverses the list returning the helper function with an empty accumulator) 
template <typename T>
std::shared_ptr<const LL<T>> reverseL(const std::shared_ptr<const LL<T>>& list) {
    return reverseLHelper(list, std::shared_ptr<const LL<T>>(nullptr)); // Start with an empty accumulator
}

//maxiHelper (helper function to find the maximum element)
template <typename T>
T maxiHelper(const std::shared_ptr<const LL<T>>& list, T currentMax) {
    if (!list) return currentMax; 
    return maxiHelper(list->next, list->value > currentMax ? list->value : currentMax); // it compares the current value with the current maximum value and returns the maximum
}

//maxi (uses the helper function to find the maximum element)
template <typename T>
T maxi(const std::shared_ptr<const LL<T>>& list) {
    if (!list) throw std::out_of_range("Cannot find maximum of an empty list"); // empty list
    return maxiHelper(list->next, list->value); // Uses the helper function to find the maximum and sets the accumulator as the first element
}

//miniHelper (helper function to find the minimum element)
template <typename T>
T miniHelper(const std::shared_ptr<const LL<T>>& list, T currentMin) {
    if (!list) return currentMin;  // Base case: empty list
    return miniHelper(list->next, list->value < currentMin ? list->value : currentMin);// it compares the current value with the current minimum value and returns the minimum
}

//mini (uses the helper function to find the minimum element)
template <typename T>
T mini(const std::shared_ptr<const LL<T>>& list) {
    if (!list) throw std::out_of_range("Cannot find minimum of an empty list"); // empty list
    return miniHelper(list->next, list->value); // Uses the helper function to find the minimum and sets the accumulator as the first element
}

//mergeTwoSorted (merges two sorted lists)
template <typename T>
std::shared_ptr<const LL<T>> mergeTwoSorted(const std::shared_ptr<const LL<T>>& list1,
                                            const std::shared_ptr<const LL<T>>& list2) {
    if (!list1) return list2; // Base case: if the first list is empty, return the second list
    if (!list2) return list1; // Base case: if the second list is empty, return the first list

    if (list1->value < list2->value) {
        return createNode(list1->value, mergeTwoSorted(list1->next, list2)); // Recursive case: add the smaller value and recurse
    } else {
        return createNode(list2->value, mergeTwoSorted(list1, list2->next)); // Recursive case: add the smaller value and recurse
    }
}

//split (splits the list into two halves, used for merge sort)
template <typename T>
std::tuple<std::shared_ptr<const LL<T>>, std::shared_ptr<const LL<T>>>
split(const std::shared_ptr<const LL<T>>& list) {
    // Base case: empty list
    if (!list) return {nullptr, nullptr};

    // Base case: single-node list
    if (!list->next) return {list, nullptr};

    // Recursive case: deconstruct the next elements and build the two split lists
    return {
        createNode(list->value, std::get<0>(split(list->next->next))),      // it puts one element in the first half and the following element in the second half
        createNode(list->next->value, std::get<1>(split(list->next->next))) 
    };
}

//mergeSort (sorts the list using mergeTwoSorted and split)
template <typename T>
std::shared_ptr<const LL<T>> mergeSort(const std::shared_ptr<const LL<T>>& list) {
    if (!list || !list->next) return list; // Base case: empty list or single-node list

    // Split the list into two halves using recursive deconstruction of the split result
    return mergeTwoSorted(
        mergeSort(std::get<0>(split(list))), // Recursively sort the first half
        mergeSort(std::get<1>(split(list)))  // Recursively sort the second half
    );
}



// Utility function to measure the execution time of a function
template <typename Func>
auto measureTime(Func&& func) {
    auto start = std::chrono::high_resolution_clock::now();
    auto result = func();
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    return std::make_tuple(result, elapsed.count());
}

// Function to read numbers from a file
std::vector<int> readNumbersFromFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        throw std::runtime_error("Could not open file: " + filename);
    }
    
    std::vector<int> numbers;
    int number;
    while (file >> number) {
        numbers.push_back(number);
    }
    return numbers;
}
// TESTING EXECUTION TIME 
int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Please provide the path to the input text file." << std::endl;
        return 1;
    }
    try {
        double time = 0;
        // Step 1: Read numbers from the file
        std::string filename = argv[1];
        std::cout << "=== Reading Numbers from File ===" << std::endl;
        auto [numberList, readTime] = measureTime([&]() { return readNumbersFromFile(filename); });
        time += readTime;

        // Step 2: Create a Linked List from the vector
        std::cout << "=== Creating Linked List ===" << std::endl;
        auto [linkedList, creationTime] = measureTime([&]() {
            std::shared_ptr<const LL<int>> list = nullptr;
            for (int num : numberList) {
                list = addLast(list, num);
            }
            return list;
        });
        std::cout << "Linked list created in: " << creationTime << " seconds." << std::endl;
        time += creationTime;

        // Step 3: Display the list
        std::cout << "=== Displaying Linked List ===" << std::endl;
        auto [_, displayTime] = measureTime([&]() { 
            std::cout << display(linkedList) << std::endl; 
            return 0; 
        });
        std::cout << "List displayed in: " << displayTime << " seconds." << std::endl;
        time += displayTime;

        // Step 4: Add an Element
        std::cout << "=== Adding Element ===" << std::endl;
        auto [listWithAdded, addTime] = measureTime([&]() { return addLast(linkedList, 50); });
        std::cout << "Added element '50' in: " << addTime << " seconds." << std::endl;
        time += addTime;

        // Step 5: Remove an Element
        std::cout << "=== Removing Element ===" << std::endl;
        auto [listWithRemoved, removeTime] = measureTime([&]() { return removeLast(listWithAdded); });
        std::cout << "Removed last element in: " << removeTime << " seconds." << std::endl;
        time += removeTime;

        // Step 6: Reverse the Linked List
        std::cout << "=== Reversing Linked List ===" << std::endl;
        auto [reversedList, reverseTime] = measureTime([&]() { return reverseL(listWithRemoved); });
        std::cout << "Reversed list in: " << reverseTime << " seconds." << std::endl;
        time += reverseTime;

        // Step 7: Display Linked List Properties
        std::cout << "=== Linked List Properties ===" << std::endl;
        auto [listSize, sizeTime] = measureTime([&]() { return size(linkedList); });
        std::cout << "Size of list calculated in: " << sizeTime << " seconds." << std::endl;
        std::cout << "Total number of nodes: " << listSize << std::endl;
        time += sizeTime;

        auto [maxValue, maxTime] = measureTime([&]() { return maxi(linkedList); });
        std::cout << "Maximum value found in: " << maxTime << " seconds." << std::endl;
        std::cout << "Maximum value: " << maxValue << std::endl;
        time += maxTime;

        auto [minValue, minTime] = measureTime([&]() { return mini(linkedList); });
        std::cout << "Minimum value found in: " << minTime << " seconds." << std::endl;
        std::cout << "Minimum value: " << minValue << std::endl;
        time += minTime;

        std::cout << "=== Finished Testing ===" << std::endl;
        std::cout << "Total time taken: " << time << " seconds." << std::endl;

    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }
    return 0;
}


// TESTING THE FUNCTIONS


// int main() {
//     // Create the initial list
//     // Using createNode to create a multi-node list
//     std::shared_ptr<const LL<int>> list = createNode(1, createNode(2, createNode(3, createNode(4, createNode(5)))));


//     // Display the original list
//     std::cout << "Original list: " << display(list) << std::endl;

//     // Add more elements (each step results in a new list)
//     auto list1 = addFirst(list, 0);
//     std::cout << "After addFirst(0): " << display(list1) << std::endl;

//     auto list2 = addLast(list1, 6);
//     std::cout << "After addLast(6): " << display(list2) << std::endl;

//     auto list3 = addAtIndex(list2, 9, 2);
//     std::cout << "After addAtIndex(9, 2): " << display(list3) << std::endl;

//     // Remove elements (each step results in a new list)
//     auto list4 = removeFirst(list3);
//     std::cout << "After removeFirst: " << display(list4) << std::endl;

//     auto list5 = removeLast(list4);
//     std::cout << "After removeLast: " << display(list5) << std::endl;

//     auto list6 = removeAtIndex(list5, 2);
//     std::cout << "After removeAtIndex(2): " << display(list6) << std::endl;

//     // Calculate the size of the list and the some of its elements
//     std::cout << "Size of list: "<< display(list6)<<" = "<< size(list6) << std::endl;
//     std::cout << "Sum of elements of the list: " <<display(list6)<<" = "<< sum(list6) << std::endl;

//     // Check for existence of elements
//     std::cout << "Does number 5 exist in: "<<display(list6) <<" ? -> "<< (check(list6, 5) ? "Yes" : "No") << std::endl;
//     std::cout << "Does number 7 exist in: "<<display(list6) <<" ? -> "<< (check(list6, 7) ? "Yes" : "No") << std::endl;

//     // Find indices of elements
//     std::cout << "Index of 9 in the list: "<<display(list6) <<" ="<< index(list6, 9) << std::endl;
//     std::cout << "Index of 7 in the list: "<<display(list6) <<" ="<< index(list6, 7) << std::endl;

//     // Map, filter (each producing a new list)  
//     auto squaredList = map(list6, std::function<int(int)>([](int x) { return x * x; }));
//     std::cout << "Mapping square function on the list: "<<display(list6) <<" Result : "<< display(squaredList) << std::endl;

//     auto filteredList = filterLL(squaredList, std::function<bool(int)>([](int x) { return x % 2 == 0; }));
//     std::cout << "Filtering the list to keep even numbers from the list: " << display(squaredList) << " Result= " << display(filteredList) << std::endl;
    
//     //foldLeft, foldright
//     int sumLeft = foldLeft<int, int>([](int acc, int x) { return acc + x; }, 0, squaredList);
//     std::cout << "FoldLeft sum of the list: " << display(squaredList) << " = " << sumLeft << std::endl;

//     int productRight = foldRight<int, int>([](int x, int acc) { return x * acc; }, 1, squaredList);
//     std::cout << "FoldRight product of the list: " << display(squaredList) << " = " << productRight << std::endl;

//     // Reverse the list
//     auto reversedList = reverseL(squaredList);
//     std::cout << "Reversing the list: "<<display(squaredList) <<" Result = "<< display(reversedList) << std::endl;

//     // Find maximum and minimum
//     std::cout << "Maximum element of the list: "<<display(reversedList) <<" = "<< maxi(reversedList) << std::endl;
//     std::cout << "Minimum element of the list: "<<display(reversedList) <<" = " << mini(reversedList) << std::endl;

//     //MergeSort
//     auto sortedList = mergeSort(reversedList);
//     std::cout << "Sorting the list: "<<display(reversedList)<< " using MergeSort: "  << display(sortedList) << std::endl;

//     // Split and merge two sorted lists
//     auto [firstHalf, secondHalf] = split(sortedList);
//     std::cout <<"Splitting "<<display(sortedList) <<" : First half: " << display(firstHalf) << "/ Second half: " << display(secondHalf) << std::endl;

//     auto mergedList = mergeTwoSorted(firstHalf, secondHalf);
//     std::cout << "Testing MergeTwoSorted on "<< display(firstHalf) <<" and "<< display(secondHalf) << " :"<< std::endl;
//     std::cout << "Merged list: " << display(mergedList) << std::endl;


//     return 0;
// }


