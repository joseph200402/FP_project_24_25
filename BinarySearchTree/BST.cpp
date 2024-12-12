// Author : Joseph Doumit Bader Tarabay
// USJ ID : 221360


// Binary Search Tree

// Functions:
// 1. emptyTree: Create an empty tree
// 2. singleTree: Create a tree with a single element
// 3. addElement: Add an element to the BST

// 4. fromList: Create a tree from a list of elements
// 5. toList: Convert the tree to a sorted list (in-order traversal)

// 6. fromSortedList: Balance a BST from a sorted list
// 7. fromUnsortedList: Create a balanced BST from an unsorted list
// 8. balance: Balance the BST

// 9. sumTree: Calculate the sum of all elements in the tree
// 10. productTree: Calculate the product of all elements in the tree

// 11. minElement: Find the minimum element in the tree
// 12. maxElement: Find the maximum element in the tree

// 13. count: Count the total number of nodes in the tree
// 14. depth: Calculate the depth of the tree

// 15. contains: Check if an element exists in the tree

// 16. removeMin: Remove the smallest element and return the updated tree
// 17. removeMax: Remove the largest element and return the updated tree
// 18. deleteElement: Delete an element from the tree

// 19. inOrderList: In-order traversal
// 20. preOrderList: Pre-order traversal
// 21. postOrderList: Post-order traversal

// 22. filterTree: Filter the BST and ensure the resulting tree is balanced
// 23. mapTree: Map a function over the tree and ensure the resulting tree is balanced

// 24. showTree: Custom Show instance for BST


// Include necessary headers
#include <iostream>
#include <memory>
#include <functional>
#include <vector>
#include <stdexcept>
#include <algorithm>
#include <string>
#include <fstream>
#include <sstream>
#include <chrono>

// Utility function to read numbers from a file
std::vector<int> readNumbersFromFile(const std::string& filename) {
    std::vector<int> numbers;
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open file: " + filename);
    }

    int num;
    while (file >> num) {
        numbers.push_back(num);
    }
    return numbers;
}

// Function to measure the time taken by an operation
template <typename Func>
auto measureTime(Func&& func) {
    auto start = std::chrono::high_resolution_clock::now();
    auto result = func();
    auto end = std::chrono::high_resolution_clock::now();
    double duration = std::chrono::duration<double>(end - start).count();
    return std::make_pair(result, duration);
}



// ______________________________________________BinarySearchTree____________________________________________


// Define the Binary Search Tree node
template <typename T>
struct BST {
    T value;
    std::shared_ptr<const BST> left;
    std::shared_ptr<const BST> right;

    BST(T val, std::shared_ptr<const BST> l = nullptr, std::shared_ptr<const BST> r = nullptr)
        : value(val), left(l), right(r) {}
};

// Utility function to create a new tree node
template <typename T>
std::shared_ptr<const BST<T>> createNode(T value,
                                         std::shared_ptr<const BST<T>> left = nullptr,
                                         std::shared_ptr<const BST<T>> right = nullptr) {
    return std::make_shared<const BST<T>>(value, left, right);
}

// In-order traversal
template <typename T>
std::vector<T> toList(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return {};
    auto leftList = toList(tree->left);
    auto rightList = toList(tree->right);
    leftList.push_back(tree->value);
    leftList.insert(leftList.end(), rightList.begin(), rightList.end());
    return leftList;
}

// Add element to the BST
template <typename T>
std::shared_ptr<const BST<T>> addElement(const std::shared_ptr<const BST<T>>& tree, T value) {
    if (!tree) return createNode(value);
    if (value < tree->value) {
        return createNode(tree->value, addElement(tree->left, value), tree->right);
    } else {
        return createNode(tree->value, tree->left, addElement(tree->right, value));
    }
}

// Create a balanced BST from a sorted list
template <typename T>
std::shared_ptr<const BST<T>> fromSortedList(const std::vector<T>& values, int start, int end) {
    if (start > end) return nullptr;

    // Find the middle index
    int mid = start + (end - start) / 2;

    // Create the node with the middle element
    auto leftSubtree = fromSortedList(values, start, mid - 1);
    auto rightSubtree = fromSortedList(values, mid + 1, end);
    return createNode(values[mid], leftSubtree, rightSubtree);
}

// Create a balanced BST from an unsorted list
template <typename T>
std::shared_ptr<const BST<T>> fromList(const std::vector<T>& values) {
    if (values.empty()) return nullptr;

    // Ensure the input list is sorted
    std::vector<T> sortedValues = values;
    std::sort(sortedValues.begin(), sortedValues.end());

    return fromSortedList(sortedValues, 0, sortedValues.size() - 1);
}


// Calculate the sum of elements in the tree
template <typename T>
T sumTree(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return 0;
    return tree->value + sumTree(tree->left) + sumTree(tree->right);
}

// Calculate the product of elements in the tree
template <typename T>
T productTree(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return 1;
    return tree->value * productTree(tree->left) * productTree(tree->right);
}

// Find the minimum element in the tree
template <typename T>
T minElement(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) throw std::out_of_range("Tree is empty");
    if (!tree->left) return tree->value;
    return minElement(tree->left);
}

// Find the maximum element in the tree
template <typename T>
T maxElement(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) throw std::out_of_range("Tree is empty");
    if (!tree->right) return tree->value;
    return maxElement(tree->right);
}

// Count the total number of nodes
template <typename T>
int count(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return 0;
    return 1 + count(tree->left) + count(tree->right);
}

// Calculate the depth of the tree
template <typename T>
int depth(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return 0;
    return 1 + std::max(depth(tree->left), depth(tree->right));
}

// Check if an element exists in the tree
template <typename T>
bool contains(const std::shared_ptr<const BST<T>>& tree, T value) {
    if (!tree) return false;
    if (tree->value == value) return true;
    if (value < tree->value) return contains(tree->left, value);
    return contains(tree->right, value);
}

// Pre-order traversal
template <typename T>
std::vector<T> preOrderList(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return {};
    auto leftList = preOrderList(tree->left);
    auto rightList = preOrderList(tree->right);
    std::vector<T> result = {tree->value};
    result.insert(result.end(), leftList.begin(), leftList.end());
    result.insert(result.end(), rightList.begin(), rightList.end());
    return result;
}

// Post-order traversal
template <typename T>
std::vector<T> postOrderList(const std::shared_ptr<const BST<T>>& tree) {
    if (!tree) return {};
    auto leftList = postOrderList(tree->left);
    auto rightList = postOrderList(tree->right);
    leftList.insert(leftList.end(), rightList.begin(), rightList.end());
    leftList.push_back(tree->value);
    return leftList;
}

// MapTree function
template <typename T, typename U>
std::shared_ptr<const BST<U>> mapTree(const std::shared_ptr<const BST<T>>& tree, std::function<U(T)> f) {
    if (!tree) return nullptr;

    // Map the function to each element in the in-order list
    std::vector<U> mappedValues;
    auto inOrder = toList(tree);
    std::transform(inOrder.begin(), inOrder.end(), std::back_inserter(mappedValues), f);

    // Sort the mapped values to maintain BST property
    std::sort(mappedValues.begin(), mappedValues.end());

    // Reconstruct the BST from the sorted list
    return fromList(mappedValues);
}

// Delete an element from the BST
template <typename T>
std::shared_ptr<const BST<T>> deleteElement(const std::shared_ptr<const BST<T>>& tree, T value) {
    if (!tree) return nullptr;

    if (value < tree->value) {
        return createNode(tree->value, deleteElement(tree->left, value), tree->right);
    } else if (value > tree->value) {
        return createNode(tree->value, tree->left, deleteElement(tree->right, value));
    } else {  // value == tree->value
        if (!tree->left) return tree->right; // No left child
        if (!tree->right) return tree->left; // No right child

        // Replace with the smallest value in the right subtree
        T minVal = minElement(tree->right);
        return createNode(minVal, tree->left, deleteElement(tree->right, minVal));
    }
}

// Filter elements of the BST based on a predicate
template <typename T>
std::shared_ptr<const BST<T>> filterTree(const std::shared_ptr<const BST<T>>& tree, std::function<bool(T)> predicate) {
    if (!tree) return nullptr;

    auto leftFiltered = filterTree(tree->left, predicate);
    auto rightFiltered = filterTree(tree->right, predicate);

    if (predicate(tree->value)) {
        return createNode(tree->value, leftFiltered, rightFiltered);
    } else {
        // Merge left and right filtered subtrees
        std::vector<T> mergedValues = toList(leftFiltered);
        auto rightValues = toList(rightFiltered);
        mergedValues.insert(mergedValues.end(), rightValues.begin(), rightValues.end());
        return fromList(mergedValues); // Create a balanced tree from merged values
    }
}

// Display a tree as a string
template <typename T>
void displayTree(const std::shared_ptr<const BST<T>>& tree, int depth = 0) {
    if (!tree) return;
    displayTree(tree->right, depth + 1);
    std::cout << std::string(4 * depth, ' ') << tree->value << "\n";
    displayTree(tree->left, depth + 1);
}


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
        // std::cout << "Numbers read in: " << readTime << " seconds." << std::endl;
        time += readTime;

        // Step 2: Create a BST from the list
        std::cout << "=== Creating Tree === ";
        auto [tree, creationTime] = measureTime([&]() { return fromList<int>(numberList); });
        std::cout << "Tree created in: " << creationTime << " seconds." << std::endl;
        time += creationTime;

        // Step 3: Perform Traversals
        std::cout << "=== Traversals ===";
        auto [_, inOrderTime] = measureTime([&]() { return toList(tree); });
        std::cout << "In-order Traversal completed in: " << inOrderTime << " seconds." << std::endl;
        time += inOrderTime;

        // Step 4: Add an Element
        std::cout << "=== Adding Element ===" << std::endl;
        auto [treeWithAdded, addTime] = measureTime([&]() { return addElement(tree, 50); });
        std::cout << "Added element '50' in: " << addTime << " seconds." << std::endl;
        time += addTime;

        // Step 5: Remove an Element
        std::cout << "=== Removing Element === ";
        auto [treeWithRemoved, removeTime] = measureTime([&]() { return deleteElement(treeWithAdded, 50); });
        std::cout << "Removed element '50' in: " << removeTime << " seconds." << std::endl;
        time += removeTime;

        // Step 6: Balance the Tree
        std::cout << "=== Balancing Tree === ";
        auto [balancedTree, balanceTime] = measureTime([&]() { return fromList(toList(treeWithRemoved)); });
        std::cout << "Balanced tree in: " << balanceTime << " seconds." << std::endl;
        time += balanceTime;

        // Step 7: Display Tree Properties
        std::cout << "=== Tree Properties === ";
        auto [nodeCount, countTime] = measureTime([&]() { return count(tree); });
        std::cout << "Counted nodes in: " << countTime << " seconds." << std::endl;
        std::cout << "Total number of nodes: " << nodeCount << std::endl;
        time += countTime;

        auto [treeDepth, depthTime] = measureTime([&]() { return depth(tree); });
        std::cout << "Depth of Tree calculated in: " << depthTime << " seconds.";

        std::cout << "=== Finished Testing === " << std::endl;
        std::cout << "Total time taken: " << time << " seconds." << std::endl;

    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }
    return 0;
}
