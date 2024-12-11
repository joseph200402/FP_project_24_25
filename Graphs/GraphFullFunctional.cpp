// Sophia Jarjoura 
// USJ ID : 220590

#include <iostream>
#include <vector>
#include <memory>
#include <algorithm>
#include <functional>
#include <chrono>

using namespace std;

// Template class for Graph
template <typename T>
class Graph {
    int vertices;                              // Number of vertices
    vector<vector<T>> adjList;                 // Adjacency list

public:
    Graph() : vertices(0) {}                   // Default constructor

    // Constructor with predefined vertices
    explicit Graph(int v) : vertices(v), adjList(v) {}

    // Add a vertex
    void addVertex() {
        adjList.push_back({});
        ++vertices;
    }

    // Add an edge between u and v
    void addEdge(T u, T v) {
        if (u >= vertices || v >= vertices) {
            throw out_of_range("Vertex does not exist");
        }
        adjList[u].push_back(v);
    }

    // Remove a vertex
    void removeVertex(T v) {
        if (v >= vertices) {
            throw out_of_range("Vertex does not exist");
        }

        // Remove the vertex itself
        adjList.erase(adjList.begin() + v);
        --vertices;

        // Adjust edges
        for (auto& neighbors : adjList) {
            neighbors.erase(remove(neighbors.begin(), neighbors.end(), v), neighbors.end());
            for (auto& neighbor : neighbors) {
                if (neighbor > v) {
                    --neighbor;
                }
            }
        }
    }

    // Remove an edge
    void removeEdge(T u, T v) {
        if (u >= vertices) {
            throw out_of_range("Vertex does not exist");
        }
        adjList[u].erase(remove(adjList[u].begin(), adjList[u].end(), v), adjList[u].end());
    }

    // Print the graph
    void printGraph() const {
        for (int i = 0; i < vertices; ++i) {
            cout << "Vertex " << i << ": ";
            for (const auto& neighbor : adjList[i]) {
                cout << neighbor << " ";
            }
            cout << "\n";
        }
    }

    // DFS traversal
    void dfs(T start) const {
        vector<bool> visited(vertices, false);
        function<void(T)> dfsUtil = [&](T v) {
            visited[v] = true;
            cout << v << " ";
            for (const auto& neighbor : adjList[v]) {
                if (!visited[neighbor]) {
                    dfsUtil(neighbor);
                }
            }
        };
        dfsUtil(start);
        cout << "\n";
    }

    // BFS traversal
    void bfs(T start) const {
        vector<bool> visited(vertices, false);
        vector<T> queue;
        visited[start] = true;
        queue.push_back(start);

        while (!queue.empty()) {
            T v = queue.front();
            queue.erase(queue.begin());
            cout << v << " ";
            for (const auto& neighbor : adjList[v]) {
                if (!visited[neighbor]) {
                    visited[neighbor] = true;
                    queue.push_back(neighbor);
                }
            }
        }
        cout << "\n";
    }

    // Measure time to remove a vertex
    void benchmarkRemoveVertex(T v) {
        auto start = chrono::high_resolution_clock::now();
        removeVertex(v);
        auto end = chrono::high_resolution_clock::now();
        cout << "Time to remove vertex " << v << ": "
             << chrono::duration_cast<chrono::microseconds>(end - start).count()
             << " microseconds\n";
    }
};

// Main function to demonstrate the functionality
int main() {
    Graph<int> g(4);
    g.addEdge(0, 1);
    g.addEdge(0, 2);
    g.addEdge(1, 3);

    cout << "Graph before modifications:\n";
    g.printGraph();

    g.addVertex();
    cout << "\nAdded a vertex:\n";
    g.printGraph();

    g.addEdge(4, 1);
    cout << "\nAdded an edge from 4 to 1:\n";
    g.printGraph();

    g.removeEdge(0, 2);
    cout << "\nRemoved edge from 0 to 2:\n";
    g.printGraph();

    g.removeVertex(3);
    cout << "\nRemoved vertex 3:\n";
    g.printGraph();

    cout << "\nDFS traversal starting from vertex 0:\n";
    g.dfs(0);

    cout << "\nBFS traversal starting from vertex 0:\n";
    g.bfs(0);

    g.benchmarkRemoveVertex(2);

    cout << "\nGraph after removing vertex 2:\n";
    g.printGraph();

    return 0;
}
