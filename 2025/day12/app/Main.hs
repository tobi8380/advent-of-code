module Main where
import Data.List.Split

type Region = (Int, [Int])

parseRegion :: String -> Region
parseRegion x = (size, map read rest)
    where
        (axb : rest) = words x
        [a, b'] = splitOn "x" axb
        b = init b'
        size = read a * read b

canFitPresents :: Region -> Bool
canFitPresents (size, counts) = size >= 9 * sum counts

main :: IO Int
main = do
    contents <- readFile "input"
    let lines' = lines contents
    let regions = map parseRegion $ drop 30 lines'
    let result = length $ filter canFitPresents regions
    print result
    return result
