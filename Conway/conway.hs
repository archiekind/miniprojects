import Control.Concurrent (threadDelay)


type Cell = Bool
type Grid = [[Cell]]

dim :: Int
dim = 4

initialGrid :: Grid
initialGrid = [[False, True, False], [False, True, False], [False, True, False]]

nextState :: Grid -> Int -> Int -> Grid -> Grid
nextState [] _ dim _ = []
nextState (row:rows) x y grid = g row grid : nextState rows x (y + 1) grid

  where g :: [Cell] -> Grid -> [Cell]
        g [] _ = []
        g (True:ex) grid
          | countNeighbours x y grid < 2 = False : g ex grid
          | countNeighbours x y grid > 3 = False : g ex grid
          | otherwise = True : g ex grid
        g (False:ex) grid
          | countNeighbours x y grid == 3 =  True : g ex grid
          | otherwise = False : g ex grid
  
  
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
          | y == 0 && x == 0 = 0 + g (x - 1) y es
          | x < 2 && x > -2 && e = 1 + g (x - 1) y es
          | otherwise = 0 + g (x - 1) y es

renderGrid :: Grid -> IO ()
renderGrid = mapM_ (putStrLn . map (\c -> if c then '*' else '.'))

runGame :: Grid -> IO ()
runGame grid = do
  renderGrid grid
  threadDelay 500000           -- Pause for half a second
  putStrLn "\ESC[2J"           -- Clear the screen
  runGame (nextState grid 0 0 grid)

makeGrid :: [String] -> Grid
makeGrid [] = []
makeGrid (xs:xss) = g xs : makeGrid xss
  where g :: [Char] -> [Cell]
        g [] = []
        g (x:xs) 
          | x == '1' = True : g xs
          | otherwise = False : g xs

initialise :: IO ()
initialise = do
  gridStr <- readFile "grid.txt"
  runGame (makeGrid (lines gridStr))
