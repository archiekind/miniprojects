

type Cell = Bool
type Grid = [[Cell]]

dim :: Int
dim = 3

initialGrid :: Grid
initialGrid = [[False, True, False], [False, True, False], [False, True, False]]


-- nextState :: Grid -> Int -> Int -> Grid
-- nextState [] _ _ = []
-- nextState grid@(row:rows) x y = g row ++ nextState grid x (y + 1)

--   where g :: [Cell] -> grid -> [Cell]
--         g (e:ex) 
  
  
-- [[updateCell x y grid | y <- [0..dim]] | x <- [0..dim]]

-- can you implement this using foldr
countNeighbours :: Int -> Int -> Grid -> Int
countNeighbours x y [] = 0
countNeighbours x y (row:rows) 
  | y < 2 && y > -2 = g x y row + countNeighbours x (y - 1) rows            -- count matches if on a neighbouring square
  | otherwise = countNeighbours x (y - 1) rows

  where g :: Int -> Int -> [Cell] -> Int
        g x y [] = 0
        g x y (e:es)
          | y == 0 && x == 0 = 0
          | x < 2 && x > -2 && e = 1 + g (x - 1) y es
          | otherwise = 0 + g (x - 1) y es


  -- where
  --   updateCell x y = applyRules (grid !! x !! y) (countLiveNeighbors x y)
  --   countLiveNeighbors x y = sum [grid !! (x + dx) !! (y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

applyRules :: Cell -> Int -> Cell
applyRules True n  = n == 2 || n == 3  -- Alive cell stays alive if 2 or 3 neighbors
applyRules False n = n == 3           -- Dead cell becomes alive if exactly 3 neighbors
