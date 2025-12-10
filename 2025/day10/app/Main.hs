module Main where

import Data.List.Split

data Light = On | Off deriving (Show, Eq)

type Button = [Int]

data Machine = Machine
  { lights :: [Light],
    buttons :: [Button]
  }
  deriving (Show, Eq)

trimFirstAndLast :: [a] -> [a]
trimFirstAndLast = init . tail

charToLight :: Char -> Light
charToLight '#' = On
charToLight '.' = Off
charToLight _ = Off

parseButton :: String -> Button
parseButton str = map read $ splitOn "," trimmed
  where
    trimmed = trimFirstAndLast str

parseLights :: String -> [Light]
parseLights string = map charToLight charLights
  where
    charLights = trimFirstAndLast string

parseButtons :: [String] -> [Button]
parseButtons = map parseButton

parseLine :: String -> Machine
parseLine s = Machine {lights = parseLights (head parts), buttons = parseButtons (trimFirstAndLast parts)}
  where
    parts = words s

toggleLight :: Light -> Light
toggleLight Off = On
toggleLight On = Off

toggleAt :: [Light] -> Int  -> [Light]
toggleAt [] _ = []
toggleAt (x : xs) 0 = toggleLight x : xs
toggleAt (x : xs) k = x : toggleAt xs (k - 1)

-- toggleButton :: Machine -> Button -> Machine
-- toggleButton = foldl (\m i -> m {lights = toggleAt i (lights m)})
toggleButton :: [Light] -> Button -> [Light]
-- toggleButton = foldl (\ls i ->  toggleAt i (ls))
toggleButton = foldl toggleAt

-- toggleI'thButton :: Int -> Machine -> Machine
-- toggleI'thButton i m = toggleButton (buttons m !! i) m


leastNumButtons :: Machine -> Int
leastNumButtons m = 1 + minimum (map (leastNumButtons' 10000 (buttons m) (lights m) visited . toggleButton state) (buttons m))
    where
        state = replicate (length (lights m)) Off
        visited = replicate (2^(length (lights m))) False

-- interpret list of lights as binary number 
lightsToInt :: [Light] -> Int
lightsToInt [] = 0
lightsToInt (On : xs) = 1 + 2 * (lightsToInt xs)
lightsToInt (Off : xs) = 2 * (lightsToInt xs)

updateVisited :: [Bool] -> [Int] -> [Bool]
updateVisited = foldl setAt
    where
        setAt [] _ = []
        setAt (x : xs) 0 = True : xs
        setAt (x : xs) n = x : setAt xs (n-1)
-- Least num buttons 
-- leastNumButtons' :: Int -> [Button] -> [Light] ->  [Light] -> Int
-- leastNumButtons' 0 buttons' target state = 0
-- leastNumButtons' depth buttons' target state =
--     if target == state then
--         0
--     else
--         1 + minimum (map (leastNumButtons' (depth - 1) buttons' target . toggleButton state) buttons')
        -- 1 + minimum (map (leastNumButtons' (depth - 1) buttons' target) (removeDups (map (toggleButton state) buttons')))

leastNumButtons' :: Int -> [Button] -> [Light] -> [Bool] -> [Light] -> Int
leastNumButtons' 0 buttons' target visited state = 0
leastNumButtons' depth buttons' target visited state =
    if target == state then
        0
    else
        case childrenResults of 
            [] -> 0
            xs -> 1 + minimum xs
    where 
        nextStates = map (toggleButton state) buttons'
        nextStatesNoDups = filter ((visited !!) . lightsToInt) nextStates
        nextStatesInts = map lightsToInt nextStatesNoDups -- List of ints corresponding to states [00101, 11001, 01111011, ...]
        newVisited = updateVisited visited nextStatesInts

        recursive = leastNumButtons' (depth - 1) buttons' target newVisited 
        childrenResults = (map recursive nextStatesNoDups)


removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x : xs) =
    if contains (removeDups xs) x then
        removeDups xs
    else
        x : removeDups xs
    where
        contains [] _ = False
        contains (y : ys) a = (a == y) || contains ys a

-- leastNumButtons' target state = minimum $ map (leastNumButtons' target . toggleButton state) (buttons target)
-- toggleButton ((buttons m) !! i) m
main :: IO Int
main = do
  input <- readFile "input"
  let inputLines = lines input
  let machines = map parseLine inputLines
  let result = sum $ map leastNumButtons machines
  return result
