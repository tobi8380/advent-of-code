{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment (getArgs)
import Data.Map as TMap hiding (map, take)
import Data.Set as Set hiding (map, take)
import Data.Maybe
import Data.Char

import Debug.Trace

type Vertex = Int
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
parseVertex (vertex : neighbors) = (labelToInt (trimLast vertex), map labelToInt neighbors)
parseVertex [] = undefined

-- buildGraph :: [String] -> Graph
-- buildGraph labels = ([],[])

-- buildGraph' :: Graph -> [String] -> Graph
-- buildGraph' (vertices, edges) (lbl : labels) 
--  | elem lbl (map label vertices) = buildGraph' (vertices, edges) (labels) -- vertices already contains entry for lbl
--  | othe

-- sumMaybe :: [Maybe Int] -> Maybe Int
-- sumMaybe [] = Nothing
-- sumMaybe xs = Just (aux xs)
--     where 
--         aux [] = 0
--         aux ys = sum $ map (\case {Just x -> x; Nothing -> 0}) ys

countPaths :: Graph -> Set Int -> Vertex -> Vertex -> Int
countPaths g visited target source
    | source `elem` visited = 0
    | source == target = 
        if labelToInt "dac" `Set.member` visited && labelToInt "fft" `Set.member` visited then
            1
        else
            0
    -- | target `elem` nbrs = recursive -- Target is a neighbor of source
    --     >>= (\x -> return (x+1))
    | otherwise = recursive
    where
        recursive = sum $ map (countPaths g (Set.insert source visited) target) nbrs
        nbrs = neighbors g source

labelToInt :: String -> Int
labelToInt (a : b : c : []) = (ord a - ord 'a') + 26 * (ord b - ord 'a') + (26^2) * (ord c - ord 'a')
labelToInt _ = undefined

main :: IO (Int)
main = do
    -- args <- getArgs
    -- print args
    let filename = "input"
    contents <- readFile filename
    let linesVertices = map words $ lines contents

    let vertices = map parseVertex linesVertices
    let graph = TMap.fromList ((labelToInt "out", []): vertices)
    -- print vertices
    print graph
    -- print $ TMap.lookup "you" graph
    let result = countPaths graph Set.empty (labelToInt "out") (labelToInt "svr")
    print result
    return result

