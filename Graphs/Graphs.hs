-- Sophia Jarjoura 
-- USJ ID : 220590

import System.CPUTime

type Vertex = Int
type Graph = [(Vertex, [Vertex])]

createGraph :: Int -> Graph
createGraph n = [(v, []) | v <- [0..(n-1)]]

addEdge :: Graph -> Int -> Int -> Graph
addEdge graph u v = 
    map (\(vertex, neighbors) -> 
         if vertex == u 
         then (vertex, v : neighbors) 
         else (vertex, neighbors)) graph


addVertex :: Int -> Graph -> Graph
addVertex v graph = (v, []) : graph

removeVertex :: Int -> Graph -> Graph
removeVertex (Graph lst) v = Graph $ 
    -- Remove the vertex and also remove it from other adjacency lists
    map (\(x, neighbors) -> (x, filter (/= v) neighbors)) $
    filter ((/= v) . fst) lst -- Remove the vertex itself

removeEdge :: Int -> Int -> Graph -> Graph
removeEdge v1 v2 graph = 
    map (\(v, neighbors) -> 
        if v == v1 then (v, filter (/= v2) neighbors)
        else (v, neighbors)) graph


dfs :: Int -> Graph -> [Int]
dfs start graph = dfs' start graph [] where
    dfs' :: Int -> Graph -> [Int] -> [Int]
    dfs' v graph visited
        | v `elem` visited = visited
        | otherwise = v : foldl (\acc neighbor -> dfs' neighbor graph acc) visited (reverse (adjacent v graph))
        
    adjacent v graph = case lookup v graph of
        Just neighbors -> neighbors
        Nothing -> []



bfs :: Int -> Graph -> [Int]
bfs start graph = bfs' [start] [] graph where
    bfs' [] visited _ = visited
    bfs' (v:vs) visited graph
        | v `elem` visited = bfs' vs visited graph
        | otherwise = bfs' (vs ++ adjacent v graph) (visited ++ [v]) graph

    adjacent v graph = case lookup v graph of
        Just neighbors -> neighbors
        Nothing -> []

printGraph :: Graph -> IO ()
printGraph graph = mapM_ (\(vertex, neighbors) -> putStrLn ("Vertex " ++ show vertex ++ " -> " ++ show neighbors)) graph


timeOperation :: IO a -> IO ()
timeOperation action = do
    start <- getCPUTime
    action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) :: Double  -- Convert to seconds
    putStrLn $ "Operation took: " ++ show diff ++ " seconds"