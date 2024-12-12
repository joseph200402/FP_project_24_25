// Author : Joseph Doumit Bader Tarabay
// USJ ID : 221360
// Date : 27/02/2004


// !!!!!!!!! This implementation is not based on functional programming !!!!!!!
// !!!!!!!!! We will use it to compare the performance with the functional version !!!!!!!

// Binary Search Tree implementation in C++


#include <iostream>
#include <vector>
#include <algorithm>
#include <stdexcept>
#include <fstream>
#include <sstream>
#include <chrono>

class BinarySearchTree {
private:
    struct Node {
        int value;
        Node* left;
        Node* right;

        Node(int val) : value(val), left(nullptr), right(nullptr) {}
    };

    Node* root;

    // Helper function for adding a value
    Node* addElement(Node* node, int value) {
        if (!node) return new Node(value);
        if (value < node->value) {
            node->left = addElement(node->left, value);
        } else {
            node->right = addElement(node->right, value);
        }
        return node;
    }

    // Helper function for in-order traversal
    void inOrderTraversal(Node* node, std::vector<int>& result) const {
        if (!node) return;
        inOrderTraversal(node->left, result);
        result.push_back(node->value);
        inOrderTraversal(node->right, result);
    }

    // Helper function for calculating the sum of the tree
    int sumTree(Node* node) const {
        if (!node) return 0;
        return node->value + sumTree(node->left) + sumTree(node->right);
    }

    // Helper function for calculating the product of the tree
    int productTree(Node* node) const {
        if (!node) return 1;
        return node->value * productTree(node->left) * productTree(node->right);
    }

    // Helper function for finding the minimum element
    int minElement(Node* node) const {
        if (!node) throw std::runtime_error("Tree is empty");
        while (node->left) {
            node = node->left;
        }
        return node->value;
    }

    // Helper function for finding the maximum element
    int maxElement(Node* node) const {
        if (!node) throw std::runtime_error("Tree is empty");
        while (node->right) {
            node = node->right;
        }
        return node->value;
    }

    // Helper function for counting nodes
    int countNodes(Node* node) const {
        if (!node) return 0;
        return 1 + countNodes(node->left) + countNodes(node->right);
    }

    // Helper function for calculating depth
    int depth(Node* node) const {
        if (!node) return 0;
        return 1 + std::max(depth(node->left), depth(node->right));
    }

    // Helper function for deleting a value
    Node* deleteElement(Node* node, int value) {
        if (!node) return nullptr;

        if (value < node->value) {
            node->left = deleteElement(node->left, value);
        } else if (value > node->value) {
            node->right = deleteElement(node->right, value);
        } else {
            if (!node->left) {
                Node* temp = node->right;
                delete node;
                return temp;
            } else if (!node->right) {
                Node* temp = node->left;
                delete node;
                return temp;
            }
            int minVal = minElement(node->right);
            node->value = minVal;
            node->right = deleteElement(node->right, minVal);
        }
        return node;
    }

    // Helper function to clear the tree
    void clear(Node* node) {
        if (!node) return;
        clear(node->left);
        clear(node->right);
        delete node;
    }

public:
    // Constructor
    BinarySearchTree() : root(nullptr) {}

    // Destructor
    ~BinarySearchTree() {
        clear(root);
    }

    // Add a value to the BST
    void addElement(int value) {
        root = addElement(root, value);
    }

    // In-order traversal
    std::vector<int> inOrderTraversal() const {
        std::vector<int> result;
        inOrderTraversal(root, result);
        return result;
    }

    // Calculate the sum of the tree
    int sumTree() const {
        return sumTree(root);
    }

    // Calculate the product of the tree
    int productTree() const {
        return productTree(root);
    }

    // Find the minimum element
    int minElement() const {
        return minElement(root);
    }

    // Find the maximum element
    int maxElement() const {
        return maxElement(root);
    }

    // Count the number of nodes
    int countNodes() const {
        return countNodes(root);
    }

    // Calculate the depth of the tree
    int depth() const {
        return depth(root);
    }

    // Delete a value from the BST
    void deleteElement(int value) {
        root = deleteElement(root, value);
    }

    // Clear the tree
    void clear() {
        clear(root);
        root = nullptr;
    }
};

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

// Function to measure time taken by a function
template <typename Func>
auto measureTime(Func&& func) {
    auto start = std::chrono::high_resolution_clock::now();
    auto result = func();
    auto end = std::chrono::high_resolution_clock::now();
    double duration = std::chrono::duration<double>(end - start).count();
    return std::make_pair(result, duration);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Please provide the path to the input text file." << std::endl;
        return 1;
    }

    try {
        double totalTime = 0.0; // Track the total execution time

        // Step 1: Read numbers from the file
        std::string filename = argv[1];
        auto [numberList, readTime] = measureTime([&]() { return readNumbersFromFile(filename); });
        totalTime += readTime;

        // Step 2: Create the BST
        BinarySearchTree bst;
        auto [_, creationTime] = measureTime([&]() {
            for (const auto& num : numberList) {
                bst.addElement(num);
            }
            return 0;
        });
        totalTime += creationTime;

        // Step 3: Perform tree operations
        auto [sum, sumTime] = measureTime([&]() { return bst.sumTree(); });
        totalTime += sumTime;

        auto [product, productTime] = measureTime([&]() { return bst.productTree(); });
        totalTime += productTime;

        auto [minVal, minTime] = measureTime([&]() { return bst.minElement(); });
        totalTime += minTime;

        auto [maxVal, maxTime] = measureTime([&]() { return bst.maxElement(); });
        totalTime += maxTime;

        auto [nodeCount, countTime] = measureTime([&]() { return bst.countNodes(); });
        totalTime += countTime;

        auto [treeDepth, depthTime] = measureTime([&]() { return bst.depth(); });
        totalTime += depthTime;

        // Final output
        std::cout << "=== Finished Testing ===" << std::endl;
        std::cout << "Tree created with " << nodeCount << " nodes." << std::endl;
        std::cout << "Tree depth: " << treeDepth << std::endl;
        std::cout << "Minimum value: " << minVal << std::endl;
        std::cout << "Maximum value: " << maxVal << std::endl;
        std::cout << "Sum of tree: " << sum << std::endl;
        std::cout << "Product of tree: " << product << std::endl;
        std::cout << "Total time taken: " << totalTime << " seconds." << std::endl;

    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }

    return 0;
}
