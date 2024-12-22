import Graphics.Gloss

type Cell = Bool
type Grid = [[Cell]]

xdim, ydim :: Int
xdim = 11
ydim = 18

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
    safeGet g a b 
      | a < 0 || b < 0 = False
      | b >= length g = False
      | a >= length (head g) = False
      | otherwise = g !! b !! a

window :: Display
window = InWindow "Game of Life" (pixelSize * xdim, pixelSize * ydim) (200, 200)

background :: Color
background = black

rectangle :: Picture
rectangle = color white (rectangleSolid (fromIntegral pixelSize) (fromIntegral pixelSize))

drawing :: Grid -> [Picture]
drawing [] = []
drawing (row:rows) = g row (ydim `div` 2 - length rows) ++ drawing rows
  where g :: [Cell] -> Int -> [Picture]
        g [] _ = []
        g (True:xs) d = translate (fromIntegral ((xdim `div` 2 - length xs) * pixelSize)) (fromIntegral(d * pixelSize)) rectangle : g xs d
        g (False:xs) d = g xs d

-- renderGrid :: Grid -> IO ()
-- renderGrid grid = do 
--   animate window background (\_ -> pictures (drawing grid))

makeGrid :: [String] -> Grid
makeGrid [] = []
makeGrid (xs:xss) = g xs : makeGrid xss
  where g :: [Char] -> [Cell]
        g [] = []
        g (x:xs') 
          | x == '1' = True : g xs'
          | otherwise = False : g xs'

runGame :: Grid -> IO ()
runGame initialGrid = simulate window background 2 initialGrid (pictures . drawing) (\_ _ grid -> nextState grid)

pixelSize :: Int
pixelSize = 10

main :: IO ()
main = do
  gridStr <- readFile "grid.txt"
  runGame (makeGrid (lines gridStr))

