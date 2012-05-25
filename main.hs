import qualified Data.Vector.Unboxed as V
import qualified Data.Array.Unboxed as A
import Control.Monad (forM_)

type Binary = [Int]
type Grid = A.UArray (Int,Int) Int

binaryList :: Int -> Binary
binaryList n = bs n []
    where
        bs n s | n > 1 = let (n', r) = divMod n 2 
                         in bs n' (r:s)
               | otherwise = n:s

b2i :: Binary -> Int
b2i b = sum $ zipWith (*) (map (2^) [0..]) $ reverse b

crule :: Binary -> Int
crule (s:ngb) | s == 1 = if cn == 2 || cn == 3 then 1 else 0
              | s == 0 = if cn == 3 then 1 else 0
    where
        cn = sum ngb

-- Vector representing Conway Life's Game rules
-- The index represent a state S_ij(t - dt), N_ij(t - dt) in binary format, 
-- the value at the index position is the S_ij(t) value 
conway :: V.Vector Int 
conway = V.fromList $ map (crule.s_ngb) [0..512]
    where s_ngb n = let bn = binaryList n in
                    (replicate (9 - length bn) 0) ++ bn

grid :: Int -> Grid
grid n = A.array ((0,0), (n,n)) [((i,j), 0) | i <- [0..n], j <- [0..n]]

-- Ugly, just for test
ngb :: Grid -> (Int,Int) -> Binary
ngb a (x,y) = map (a A.!) [(xl, yt), (x, yt), (xr, yt),
                           (xl, y), (xr, y),
                           (xl, yb), (x, yb), (xr, yb)]
    where xl | x == 0 = xmax
             | otherwise = x - 1
          xr | x == xmax = 0
             | otherwise = x + 1
          yt | y == 0 = ymax
             | otherwise = y - 1
          yb | y == ymax = 0
             | otherwise = y + 1
          (_, (xmax, ymax)) = A.bounds a

mkGlider :: Grid -> (Int,Int) -> Grid
mkGlider g (x,y) = g A.// [((x-1, y-1), 0), ((x, y-1), 1), ((x+1, y-1), 0),
                         ((x-1, y), 0), ((x,y), 0), ((x+1, y), 1),
                         ((x-1, y+1), 1), ((x,y+1), 1), ((x+1, y+1), 1)]

updateGrid :: Grid -> V.Vector Int -> Grid
updateGrid g v = A.array bs [((i,j), up i j) | i <- [x0..x], j <- [y0..y]] 
    where 
        bs@((x0,y0), (x,y)) = A.bounds g
        up i j = v V.! (b2i $ g A.! (i,j) : ngb g (i,j)) 

printGrid :: Grid -> IO ()
printGrid g = do
    let bs@((x0,y0), (x,y)) = A.bounds g
    forM_ [y0..y] $ \j -> do
        print $ concatMap (\i -> show $ g A.! (i,j)) [x0..x]
