import System.Environment (getArgs)
import Data.Map as TMap hiding (map, take)
import Data.Maybe

type Vertex = String
type Node = (Vertex, [Vertex])

-- Adjacency list
type Graph = Map Vertex [Vertex]

-- parseEdge :: [String] -> Edge
-- parseEdge (src : outEdges) = Edge {src = src, nbrs = }

neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = Data.Maybe.fromMaybe undefined (TMap.lookup v g)

trimLast :: String -> String
trimLast s = take (length s - 1) s

parseVertex :: [String] -> Node
parseVertex (vertex : neighbors) = (trimLast vertex, neighbors)

-- buildGraph :: [String] -> Graph
-- buildGraph labels = ([],[])

-- buildGraph' :: Graph -> [String] -> Graph
-- buildGraph' (vertices, edges) (lbl : labels) 
--  | elem lbl (map label vertices) = buildGraph' (vertices, edges) (labels) -- vertices already contains entry for lbl
--  | othe

countPaths :: Graph -> [Vertex] -> Vertex -> Vertex -> Int
countPaths g visited target source
    | source == target = 0
    | target `elem` nbrs = 1 + recursive -- Target is a neighbor of source
    | otherwise = recursive
    where
        recursive = sum $ map (countPaths (source : visited) g target) $ filter (elem visited) nbrs 
        nbrs = neighbors g source



main :: IO Int
main = do
    -- args <- getArgs
    -- print args
    let filename = "part2_example_input"
    contents <- readFile filename
    let linesVertices = map words $ lines contents

    let vertices = map parseVertex linesVertices
    let graph = fromList (("out", []): vertices)
    -- print vertices
    -- print graph
    -- print $ TMap.lookup "you" graph
    let result = countPaths graph "out" "you"
    return result
