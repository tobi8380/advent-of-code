module Main where
import Data.List.Split

data Light = On | Off deriving (Show)

type Button = [Int]

data Machine = Machine {
    lights :: [Light],
    buttons :: [Button]
} deriving (Show)

 
trimFirstAndLast :: [a] -> [a]
trimFirstAndLast = init . tail

charToLight :: Char -> Light
charToLight '#' = On
charToLight '.' = Off
charToLight _ = Off


parseButton :: String -> Button
parseButton str = map read $ splitOn "," trimmed
    where trimmed = trimFirstAndLast str

parseLights :: String -> [Light]
parseLights string = map charToLight charLights
    where charLights = trimFirstAndLast string

parseButtons :: [String] -> [Button]
parseButtons = map parseButton 

parseLine :: String -> Machine
parseLine s = Machine { lights = parseLights (head parts), buttons = parseButtons (trimFirstAndLast parts) }
    where parts = words s

leastNumButtons :: Machine -> Int
leastNumButtons s = 0

main :: IO Int
main = do
    input <- readFile "input"
    let inputLines = lines input
    let machines = map parseLine inputLines
    let result = sum $ map leastNumButtons machines
    return result

