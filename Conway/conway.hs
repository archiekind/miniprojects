import Control.Concurrent (threadDelay)


type Cell = Bool
type Grid = [[Cell]]

dim :: Int
dim = 4

initialGrid :: Grid
initialGrid = [[False, True, False], [False, True, False], [False, True, False]]

nextState :: Grid -> Grid
nextState grid = [[ evolveCell x y grid | x <- [0..width-1]] | y <- [0..height-1]]
  where
    height = length grid
    width = length (head grid)
    evolveCell x y g
      | alive && (n < 2 || n > 3) = False
      | not alive && n == 3 = True
      | otherwise = alive
      where
        alive = g !! y !! x
        n = countNeighbours x y g

countNeighbours :: Int -> Int -> Grid -> Int
countNeighbours x y grid = sum [fromEnum $ safeGet grid (x+dx) (y+dy) | dx <- [-1..1], dy <- [-1..1], not (dx == 0 && dy == 0)]
  where
    safeGet :: Grid -> Int -> Int -> Bool
    safeGet g x y 
      | x < 0 || y < 0 = False
      | y >= length g = False
      | x >= length (head g) = False
      | otherwise = g !! y !! x

renderGrid :: Grid -> IO ()
renderGrid = mapM_ (putStrLn . map (\c -> if c then '*' else '.'))

runGame :: Grid -> IO ()
runGame grid = do
  renderGrid grid
  threadDelay 500000           -- Pause for half a second
  putStrLn "\ESC[2J"           -- Clear the screen
  runGame (nextState grid)

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
