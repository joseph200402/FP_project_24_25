// Sophia Jarjoura 
// USJ ID : 220590

#include <vector>
#include <iostream>
#include <algorithm>
#include <queue>
#include <chrono>
using namespace std;
using namespace chrono;

class Graph {
    int vertices; // Number of vertices
    vector<vector<int>> adjList; // Adjacency list
public:
    // Constructor
    Graph(int v) {
        vertices = v;
        for (int i = 0; i < v; i++) {
            adjList.push_back(vector<int>());
        }
    }

    // Add an edge from u to v
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
    }
    
    void addVertex(int v) {
        adjList.push_back(vector<int>());
        vertices++;
    }

    void removeVertex(int v) {
    // Remove the vertex itself
    adjList.erase(adjList.begin() + v);
    vertices--;

    // Adjust the adjacency list to reflect the new vertex indices
    for (int i = 0; i < adjList.size(); i++) {
        // Remove any edges pointing to the removed vertex
        adjList[i].erase(remove(adjList[i].begin(), adjList[i].end(), v), adjList[i].end());
        
        // Adjust edges pointing to vertices with indices greater than the removed vertex
        for (int& neighbor : adjList[i]) {
            if (neighbor > v) {
                neighbor--; // Decrement to reflect shifted indices
            }
        }
    }
}


    void removeEdge(int u, int v) {
        adjList[u].erase(remove(adjList[u].begin(), adjList[u].end(), v), adjList[u].end());
    }

    // Print the graph
    void printGraph() {
    for (int i = 0; i < vertices; ++i) {
        cout << "Vertex " << i << " -> ";
        if (!adjList[i].empty()) {  // Check if there are neighbors
            for (int j = 0; j < adjList[i].size(); ++j) {
                cout << adjList[i][j];  // Print the neighbor
                if (j != adjList[i].size() - 1) {  // If not the last neighbor, print a comma
                    cout << ", ";
                }
            }
        }
        cout << "\n";
    }
    }

    void dfs(int start) {
        vector<bool> visited(vertices, false); // Track visited nodes
        dfsUtil(start, visited); // Helper function for DFS
    }

    // Helper function for DFS (recursive)
    void dfsUtil(int v, vector<bool>& visited) {
        visited[v] = true;  // Mark the current node as visited
        cout << v << " ";   // Print the current node

        // Visit all the neighbors of the current node
        for (int i = 0; i < adjList[v].size(); i++) {
            int neighbor = adjList[v][i];
            if (!visited[neighbor]) {
                dfsUtil(neighbor, visited); // Recur for all unvisited neighbors
            }
        }
    }
    

    void bfs(int start) {
        vector<bool> visited(vertices, false); // Track visited nodes
        queue<int> q; // Queue for BFS
        visited[start] = true;  // Mark the start node as visited
        q.push(start); // Enqueue the start node

        while (!q.empty()) {
            int v = q.front(); // Dequeue a vertex
            cout << v << " ";   // Print the current node
            q.pop();

            // Visit all the unvisited neighbors of the current node
            for (int i = 0; i < adjList[v].size(); i++) {
                int neighbor = adjList[v][i];
                if (!visited[neighbor]) {
                    visited[neighbor] = true; // Mark as visited
                    q.push(neighbor); // Enqueue the unvisited neighbor
                }
            }
        }
    }
    void benchmarkRemoveVertex(int v) {
        auto start = high_resolution_clock::now();  // Start measuring time
        removeVertex(v);  // Remove the vertex
        auto stop = high_resolution_clock::now();  // Stop measuring time
        auto duration = duration_cast<microseconds>(stop - start);  // Calculate the duration
        cout << "Time taken to remove vertex " << v << ": " << duration.count() << " microseconds\n";
    }
};

int main() {
    Graph g(4); // Create a graph with 4 vertices
    g.addEdge(0, 1);
    g.addEdge(0, 2);
    g.addEdge(1, 3);
    
    cout << "Graph before adding/removing vertices and edges:\n";
    g.printGraph(); // Display the graph
    
    // Add a vertex
    cout << "\nAdding vertex 8:\n";
    g.addVertex(8);
    g.addEdge(4, 5); // Add an edge from vertex 4 to vertex 5
    g.printGraph();

    // // Remove an edge
    cout << "\nRemoving edge from vertex 0 to vertex 2:\n";
    g.removeEdge(0, 2);
    g.printGraph();

    // // Remove a vertex
    cout << "\nRemoving vertex 3:\n";
    g.removeVertex(3);
    g.printGraph();

    // DFS traversal starting from vertex 0
    cout << "\nDFS traversal starting from vertex 0:\n";
    g.dfs(0);

    // BFS traversal starting from vertex 0
    cout << "\n\nBFS traversal starting from vertex 0:\n";
    g.bfs(0);

    g.benchmarkRemoveVertex(2);

    cout << "\nGraph after removing vertex 2:\n";
    g.printGraph();
    return 0;
}

