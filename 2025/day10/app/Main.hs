{-# LANGUAGE LambdaCase #-}
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
-- toggleButton = foldl toggleAt
toggleButton state but = reverse result 
  where
    (_, _, result) = foldl aux (0, but, []) state
    aux :: (Int, [Int], [Light]) -> Light -> (Int, [Int], [Light]) 
    aux (index, [], acc) nextLight = (index + 1, [], nextLight : acc)
    aux (index, b : bs, acc) nextLight = 
      if b == index then
        (index + 1, bs , (toggleLight nextLight) : acc)
      else
        (index + 1, b : bs, nextLight : acc)


-- toggleI'thButton :: Int -> Machine -> Machine
-- toggleI'thButton i m = toggleButton (buttons m !! i) m

findFirstValue :: [Maybe a] -> Maybe a
findFirstValue [] = Nothing
findFirstValue (Nothing : xs) = findFirstValue xs
findFirstValue (Just x : xs) = Just x

-- findFirstSatisfying :: Eq a => (a -> Bool) -> [a] -> Maybe a
-- findFirstSatisfying f [] = Nothing
-- findFirstSatisfying f (x : xs) 
--   | f x = Just x
--   | otherwise = findFirstSatisfying f xs

-- Iterative deepening DFS
leastNumButtons :: Machine -> Maybe Int
leastNumButtons m = 
  findFirstValue (map (leastNumButtons' m) [1..])
  -- >>= \x -> Just (x + 1)


-- Find the smallest number of buttons to press in order to achieve light configuration
leastNumButtons' :: Machine -> Int -> Maybe Int
leastNumButtons' m maxDepth =
  (minimumMaybe ((map (leastNumButtons'' maxDepth (buttons m) (lights m) visited . toggleButton state) (buttons m))))
  >>= \x -> Just (x + 1)
    where
        state = replicate (length (lights m)) Off
        visited = []

-- interpret list of lights as binary number 
stateToInt :: [Light] -> Int
stateToInt [] = 0
stateToInt (On : xs) = 1 + 2 * (stateToInt xs)
stateToInt (Off : xs) = 2 * (stateToInt xs)

updateVisited :: [Bool] -> [Int] -> [Bool]
updateVisited = foldl setAt
    where
        setAt [] _ = []
        setAt (x : xs) 0 = True : xs
        setAt (x : xs) n = x : setAt xs (n-1)
-- Least num buttons 
-- leastNumButtons'' :: Int -> [Button] -> [Light] ->  [Light] -> Int
-- leastNumButtons'' 0 buttons' target state = 0
-- leastNumButtons'' depth buttons' target state =
--     if target == state then
--         0
--     else
--         1 + minimum (map (leastNumButtons'' (depth - 1) buttons' target . toggleButton state) buttons')
        -- 1 + minimum (map (leastNumButtons'' (depth - 1) buttons' target) (removeDups (map (toggleButton state) buttons')))

minimumMaybe :: [Maybe Int] -> Maybe Int
-- minimumMaybe xs = Just ( minimum . map (\case {Just a -> a; Nothing -> undefined}) $ filter (/= Nothing ) xs )
minimumMaybe xs = minimum' filtered
  where 
    filtered = filter (/= Nothing) xs
    minimum' [] = Nothing
    minimum' ys = Just $ (minimum . map (\case {Just a -> a; Nothing -> undefined})) ys


leastNumButtons'' :: Int -> [Button] -> [Light] -> [Int] -> [Light] -> Maybe Int
leastNumButtons'' 0 buttons' target visited state = Nothing
leastNumButtons'' depth buttons' target visited state
 | target == state = Just 0
 | contains visited (stateToInt state) = Nothing
 | otherwise = minimumMaybe childrenResults
    >>= \x -> Just (x + 1)
    where
      nextStates = map (toggleButton state) buttons'
      -- nextStatesNoDups = filter ((visited !!) . stateToInt) nextStates
      nextStatesInts = map stateToInt nextStates -- List of ints corresponding to states [00101, 11001, 01111011, ...]
      -- newVisited = updateVisited visited [stateToInt state]
      newVisited = (stateToInt state) : visited

      recursive = leastNumButtons'' (depth - 1) buttons' target newVisited
      childrenResults = (map recursive nextStates)

contains :: Eq t => [t] -> t -> Bool
contains [] _ = False
contains (y : ys) a = (a == y) || contains ys a

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

-- leastNumButtons'' target state = minimum $ map (leastNumButtons'' target . toggleButton state) (buttons target)
-- toggleButton ((buttons m) !! i) m
main :: IO Int
main = do
  input <- readFile "input"
  let inputLines = lines input
  let machines = map parseLine inputLines
  -- print (machines)
  -- print (map leastNumButtons machines)
  let result = (sum . map ((\case {Just x -> x; Nothing -> undefined}) . leastNumButtons)) machines
  return result