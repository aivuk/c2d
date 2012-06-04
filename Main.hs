module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Array.Unboxed as A
import System.Random
import Control.Concurrent
import Control.Monad (forever, when)
import Control.Monad (forM_, forM)
import Data.Array.IO as IA

import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss

type Binary = [Int]
type Cells = IA.IOUArray (Int, Int) Int

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

ngb :: Cells -> (Int,Int) -> IO Binary
ngb g (x,y) = do
        (_, (xmax, ymax)) <- IA.getBounds g
     	let xl | x == 0 = xmax
               | otherwise = x - 1
            xr | x == xmax = 0
               | otherwise = x + 1
            yt | y == 0 = ymax
               | otherwise = y - 1
            yb | y == ymax = 0
               | otherwise = y + 1
	mapM (readArray g) [(xl, yt), (x, yt), (xr, yt),
                           (xl, y), (xr, y),
                           (xl, yb), (x, yb), (xr, yb)]

mkGlider :: Cells -> (Int,Int) -> IO ()
mkGlider g (x,y) = do
	let putCell (pos, state) = IA.writeArray g pos state
	mapM_ putCell [((x-1, y-1), 0), ((x, y-1), 1), ((x+1, y-1), 0),
                       ((x-1, y), 0), ((x,y), 0), ((x+1, y), 1),
                       ((x-1, y+1), 1), ((x,y+1), 1), ((x+1, y+1), 1)]

data World = World { cells :: Cells
                   , rules :: V.Vector Int
                   , worldWidth :: Int
                   , worldHeight :: Int
                   , cellSize :: Int
                   , cellSpace :: Int
                   , worldPeriod :: Float
                   , worldElapsedTime :: Float }

cellShape :: Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi
 = let cs	 = fromIntegral cellSize
       posX = fromIntegral posXi
       posY = fromIntegral posYi
       x1   = posX
       x2	 = posX + cs
       y1	 = posY 
       y2	 = posY + cs
   in Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
		
pictureOfCell :: Int -> (Int,Int) -> Int -> Picture
pictureOfCell cellSize (posX, posY) cellState
    | cellState == 1 = Color black (cellShape cellSize posX posY)
    | cellState == 0 = Color (greyN 0.8) (cellShape cellSize posX posY)

drawCell :: World -> Int -> (Int, Int) -> Picture
drawCell w c (x,y) 
    = let cs = fromIntegral (cellSize w)
          cp = fromIntegral (cellSpace w)
          fx = fromIntegral x * (cs + cp) + 1
          fy = fromIntegral y * (cs + cp) + 1
    in pictureOfCell (cellSize w) (fx,fy) c

drawWorld :: World -> IO Picture
drawWorld w = do
    let (windowWidth, windowHeight) = windowSizeOfWorld w
        offsetX = -fromIntegral windowWidth  / 2
        offsetY = -fromIntegral windowHeight / 2
    bs@((x0,y0), (x,y)) <- IA.getBounds $ cells w
    cellsPictures <- forM [(i,j) | i <- [x0..x], j <- [y0..y]] $ \p@(i,j) -> do
                                cellstate <- IA.readArray (cells w) p
                                let pc = drawCell w cellstate p
                                return pc
    return $ Translate offsetX offsetY $ Pictures cellsPictures

windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world
 = let cSize  = cellSize world
       cSpace = cellSpace world
       cPad   = cSize + cSpace
       height = cPad * (worldHeight world) + cSpace
       width  = cPad * (worldWidth  world) + cSpace
   in (width, height)

simulateWorld :: ViewPort -> Float -> World -> IO World
simulateWorld vp t w 
    | worldElapsedTime w >= (worldPeriod w) = do
        let v = rules w
            g = cells w
        bs@((x0,y0), (x,y)) <- IA.getBounds g
    	g_copy <- IA.newListArray bs =<< IA.getElems g
    	forM_ [(i,j) | i <- [x0..x], j <- [y0..y]] $ \pos -> do
    	 	c <- IA.readArray g_copy pos
    		c_ngb <- ngb g_copy pos
    		IA.writeArray g pos $ (rules w) V.! (b2i $ c:c_ngb) 
        return $ w 
    | otherwise = return $ w { worldElapsedTime = worldElapsedTime w + t }

grid :: Int -> IO Cells
grid n = IA.newArray ((0,0), (n,n)) 0 

newWorld = do 
    g <- grid 100
    mkGlider g (99,99)
    mkGlider g (96,96)
    mkGlider g (3,97)
    return $ World g conway 100 100 5 1 0.01 0

main = do
    let width  = 100
        height = 100
    w <- newWorld 
    simulateIO (InWindow "John Conway's Game of Life"
                    (windowSizeOfWorld w) (5,5))
        white 10 w drawWorld simulateWorld