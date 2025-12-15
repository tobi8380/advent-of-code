{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment (getArgs)
import Data.Map as TMap hiding (map, take)
import Data.Set as Set hiding (map, take)
import Data.Maybe
import Data.Char

import Debug.Trace
import qualified GHC.ForeignPtr as Data

type Vertex = Int
type Node = (Vertex, ([Vertex], Maybe Int))

-- Adjacency list
type Graph = Map Vertex ([Vertex], Maybe Int)

neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = nbrs
    where
        (nbrs, _) = Data.Maybe.fromMaybe undefined (TMap.lookup v g)

trimLast :: String -> String
trimLast s = take (length s - 1) s

parseVertex :: [String] -> Node
parseVertex (vertex : nbrs) = (labelToInt (trimLast vertex), (map labelToInt nbrs, Nothing))
parseVertex [] = undefined


setNumPathsFrom :: Graph -> Vertex -> Int -> Graph
setNumPathsFrom g v numPaths = adjust aux v g
    where
        aux (vs, _) = (vs, Just numPaths)

addNumPathsFrom :: Graph -> Vertex -> Int -> Graph
addNumPathsFrom g v addCount = adjust aux v g
    where
        aux (vs, Nothing) = (vs, Just addCount)
        aux (vs, Just pathCount) = (vs, Just (pathCount + addCount))

foundPathFrom :: Graph -> Vertex -> Graph
foundPathFrom g v = addNumPathsFrom g v 1

getNumPaths :: Graph -> Vertex -> Maybe Int
getNumPaths g source = snd $ Data.Maybe.fromMaybe undefined (TMap.lookup source g)

countPaths :: Graph -> Vertex -> Vertex -> Maybe Int
countPaths g target source = snd $ Data.Maybe.fromMaybe undefined (TMap.lookup source (countPaths' g target source))


-- NOTE: I am pretty there are no cycles in the graph, so assuming this
countPaths' :: Graph -> Vertex -> Vertex -> Graph
countPaths' g target source
    | target == source = setNumPathsFrom g source 1
    | isJust cachedResult = g
    | otherwise = setNumPathsFrom newGraph source numPaths
    where
        cachedResult = getNumPaths g source
        (newGraph, numPaths) = sumNeighbors g target (neighbors g source)


sumNeighbors :: Graph -> Vertex -> [Vertex] -> (Graph, Int)
sumNeighbors g target [] = (g, 0)
sumNeighbors g target (node : siblings) = (newNewGraph, numPaths + numPathsRest)
    where
        newGraph = countPaths' g target node
        numPaths = fromMaybe 0 $ getNumPaths newGraph node 
        (newNewGraph, numPathsRest) = sumNeighbors newGraph target siblings



labelToInt :: String -> Int
labelToInt [a, b, c] = (ord a - ord 'a') + 26 * (ord b - ord 'a') + (26^2) * (ord c - ord 'a')
labelToInt _ = undefined

main :: IO ()
main = do
    -- args <- getArgs
    -- print args
    let filename = "input"
    contents <- readFile filename
    let linesVertices = map words $ lines contents

    let vertices = map parseVertex linesVertices
    let graph = TMap.fromList ((labelToInt "out", ([], Nothing)): vertices)

    -- This only works because there are no cycles and all paths go from fft to dac
    -- So technically this solution is not very generic
    -- Could be solved by booleans on paths tracking whether dac and fft have been found
    let (Just svrToFft) = countPaths graph (labelToInt "fft") (labelToInt "svr")
    let (Just fftToDac) = countPaths graph (labelToInt "dac") (labelToInt "fft")
    let (Just dacToOut) =  countPaths graph (labelToInt "out") (labelToInt "dac")
    -- print svrToFft
    -- print fftToDac
    -- print dacToOut

    let result = svrToFft * fftToDac * dacToOut
    print result
    return ()

