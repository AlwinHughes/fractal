{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphical where

import Control.Monad
import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent 
import Data.Bits
import Data.Word
import Data.Scientific as Scientific
import Data.Complex
import GHC.Conc (numCapabilities)
import qualified Control.Concurrent.MVar.Strict as SM 
import GHC.Generics (Generic)
import Control.DeepSeq
import Mand

data Stuff a = Stuff [Word64] 
  deriving (Eq, Generic, NFData)

startGraphical :: MVar Bool -> IO ()
startGraphical mv = do 
  print "graphical started"
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  let scr = defaultScreenOfDisplay dpy
  let background = whitePixel dpy dflt
  let border = blackPixel dpy dflt
  let max = 5000
  rootw <- rootWindow dpy dflt
  win  <- createSimpleWindow dpy rootw 0 0 600 600 1 border background 
  --win <- mkUnmanagedWindow dpy scr rootw 0 0 500 500 
  setTextProperty dpy win "this will not be seen" wM_NAME
  mapWindow dpy win
  --drawKey dpy win max
  --drawManyInWin 10 10 50 ["green","yellow", "blue","black"] dpy win 0 0 
  --callNTime 50 10 10 ["green","yellow", "blue","black"] (drawManyInWin 10 10 10 dpy win)
  --drawInWin dpy win "blue"
  col <- initColor dpy "red"
  --drawAPoint 400 400 dpy win col 
  drawSetParalell dpy win (-2) (-2) 500 500 0.008 zzcos_iteration max
  
  --drawSet dpy win (-1.4) (-0.005) 500 500 0.00002 mand_iteration 
  --mapM_ (printAllCol dpy win) [(x,y) | x <- [0..255], y <- [0..255]]
  --drawRGBRec dpy win $ pixelFromRGB 150 200 25 
  --drawRGBRec dpy win $ pixelFromRGB 0 0 255
  --allocaXEvent (eventThing dpy win)
  sync dpy False
  return ()
  --threadDelay (10 * 1000000)
  --exitWith ExitSuccess
  
printAllCol :: Display -> Window -> (Int, Int) -> IO ()
printAllCol dpy win (x, y) = drawAPoint x y dpy win $ scaleUpToPixel (256*255 + 255) $ 256 * x + y 

scaleUpToPixel :: Int -> Int -> Word64
scaleUpToPixel max n = floor $ (256*256*256 -1) * ((fromIntegral n) / (fromIntegral max))

mkUnmanagedWindow :: Display -> Screen -> Window -> Position -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
 let visual = defaultVisualOfScreen scr
     attrmask = cWOverrideRedirect .|. cWBorderPixel .|. cWBackPixel
 win <- allocaSetWindowAttributes $ \attributes -> do
     set_override_redirect attributes True
     set_background_pixel attributes $ whitePixel dpy (defaultScreen dpy)
     set_border_pixel attributes $ blackPixel dpy (defaultScreen dpy)
     createWindow dpy rw x y w h 1 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes
 return win

-- 0:30
drawSet :: RealFloat a => Display -> Window -> a -> a -> Int -> Int -> a -> (Complex a -> Complex a -> Complex a) -> Int ->IO ()
drawSet dpy win startRe startIm numberRe numberIm step nextIteration max = do 
  gc <- createGC dpy win
  mapM_ (stuff gc) [(x,y) | x <- [0..(numberRe-1)] , y <- [0..(numberIm -1)] ] 
  freeGC dpy gc
    where
      --stuff ::  -> (Int, Int) -> IO ()
      stuff gc (x, y) = do 
        let re = startRe + (fromIntegral x) * step 
        let im = startIm + (fromIntegral y) * step
        --let pixel = scaleIntToRGB (general nextIteration (re :+ im) 1000) 10 
        let pixel = scaleUpToPixel (max -1) $ general nextIteration Nothing (re :+ im) max 
        setForeground dpy gc pixel
        drawPoint dpy win gc (fromIntegral x) (fromIntegral y)

drawSetParalell :: RealFloat a => Display -> Window -> a -> a -> Int -> Int -> a -> (Complex a -> Complex a -> Complex a) -> Int ->IO ()
drawSetParalell dpy win startRe startIm numberRe numberIm step nextIteration max = do
  gc <- createGC dpy win
  
  case numCapabilities of
    1 -> drawSet dpy win startRe startIm numberRe numberIm step nextIteration max
    otherwise -> do
      
      vendor <- SM.newEmptyMVar 
      ret <- SM.newEmptyMVar
      v1 <- forkIO $ vendorThread vendor startRe step max numberRe
      gc <- createGC dpy win
      t1 <- forkIO $ processingThread vendor ret startIm step nextIteration max numberIm "1"
      t2 <- forkIO $ processingThread vendor ret startIm step nextIteration max numberIm "2"
      mapM_ (\_ -> do 
        (iteration_arr, x) <- SM.takeMVar ret
        drawLineOfPoints dpy win gc x 0 iteration_arr) [1..max]
      killThread v1
      killThread t1
      killThread t2
      freeGC dpy gc

processingThread :: RealFloat a => SM.MVar (a, Int) -> SM.MVar ([Word64], Int) -> a -> a -> (Complex a -> Complex a -> Complex a) -> Int -> Int -> String -> IO ()
processingThread vendor ret im_start step nextIteration max numberIm name = do
  forever (do 
    (re, x) <- SM.takeMVar vendor 
    --print name
    SM.putMVar ret $ (calcuateLine re im_start step nextIteration max numberIm, x)) 

--vendorThread :: RealFloat a => SM.MVar (a, Int) -> a -> a -> Int -> Int -> IO () 
vendorThread vend reStart step max maxn = do mapM_ (\n -> SM.putMVar vend  (reStart + step * fromIntegral n, n)) [0.. maxn] 

calcuateLine :: RealFloat a => a -> a -> a -> (Complex a -> Complex a -> Complex a) -> Int -> Int -> [Word64]
calcuateLine re im_start step nextIteration max numberIm = 
  map (\ x -> scaleUpToPixel (max-1)  $ general nextIteration Nothing (re :+ (im_start + (fromIntegral x) * step)) max) [0..(numberIm-1)] 

drawLineOfPoints :: Display -> Window -> GC -> Int -> Int -> [Word64] -> IO ()
drawLineOfPoints dpy win gc x y [] = return ()
drawLineOfPoints dpy win gc x y (a:as) = do
  setForeground dpy gc a
  drawPoint dpy win gc (fromIntegral x) (fromIntegral y) 
  drawLineOfPoints dpy win gc x (y + 1) as
      
drawKey :: Display -> Window -> Int -> IO ()
drawKey dpy win max = do
  gc <- createGC dpy win
  mapM_ (f gc) [0..500] 
  freeGC dpy gc
  where 
    f gc p = do
      drawRGBRec' dpy win  (scaleUpToPixel max $ round (fromInteger p * ((fromIntegral max) / 500))) 200 200 200 200

      setForeground dpy gc $ scaleUpToPixel max $ round (fromInteger p * ((fromIntegral max) / 500))
      fillRectangle dpy win gc 510 (fromIntegral p) 20 1 
      print $ scaleUpToPixel max $ round (fromInteger p * ((fromIntegral max) / 500))

--the peramiter max should be the third root of the maximum value that n can take
--scaleIntToRGB n max = if n == max * max * max then 0 else pixelFromRGB c1 c2 c3
scaleIntToRGB :: Int -> Int -> Word64
scaleIntToRGB n max = pixelFromRGB c1 c2 c3
  where
    scale x = round $ 255 * ((fromIntegral x) / fromIntegral (max))
    c1 = scale $ div n (max* max)
    c2 = scale $ mod (div n max) (max * max)
    c3 = scale $ mod n max
    
    
scaleIntToWord :: Int -> Int -> Word64
scaleIntToWord max i = floor $ 16777215 * fromIntegral i / fromIntegral max

scaleInt :: Int -> Int -> Word64
scaleInt i max = fromIntegral . round $ ((fromIntegral i) / (fromIntegral max)) * (16777215 / fromIntegral max)
{-       
scaleInt2 :: Int -> Int -> Word64 
scaleInt2 i max 
  | i < mrt = fromIntegral i
  | i < mrt * 2 = fromIntegral 256 * i
  | otherwise = fromIntegral 256 * 256 * i
  where
    mrt = max ** (1/3)
-}
drawInWin :: Display -> Window -> String -> IO ()
drawInWin dpy win col = do
  fgcolour <- initColor dpy col 
  gc <- createGC  dpy win
  setForeground dpy gc fgcolour
  fillRectangle dpy win gc 0 0 50 50 
  freeGC dpy gc

drawAPoint :: Int -> Int -> Display -> Window -> Graphics.X11.Xlib.Pixel -> IO ()
drawAPoint x y dpy win pixel = do
  gc <- createGC dpy win
  setForeground dpy gc pixel 
  drawPoint dpy win gc (fromIntegral x) (fromIntegral y)
  freeGC dpy gc

drawRGBRec :: Display -> Window -> Graphics.X11.Xlib.Pixel -> IO ()
drawRGBRec dpy win pixel = do
  gc <- createGC dpy win
  setForeground dpy gc pixel
  fillRectangle dpy win gc 100 100 200 300
  freeGC dpy gc

drawRGBRec' :: Display -> Window -> Graphics.X11.Xlib.Pixel -> Int -> Int -> Int -> Int -> IO ()
drawRGBRec' dpy win pixel x y width height= do
  gc <- createGC dpy win
  setForeground dpy gc pixel
  fillRectangle dpy win gc (fromIntegral x) (fromIntegral y)  (fromIntegral width) (fromIntegral height)
  freeGC dpy gc


drawManyInWin :: Int -> Int -> Int -> Display -> Window -> Int -> Int -> [String] -> IO ()
drawManyInWin width height number dpy win x y (c:cols)= do
  col <- initColor dpy c 
  gc <- createGC dpy win
  setForeground dpy gc col
  fillRectangle dpy win gc (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
  freeGC dpy gc
  if number == 0 then return () else drawManyInWin width height (number -1) dpy win (x + width) y (cols ++ [c]) 
  
callNTime :: Int -> Int -> Int -> [String] -> (Int -> Int -> [String] -> IO ()) -> IO () 
callNTime 0 _ _ _ _ = return () 
callNTime number startx starty colar@(c:cols) mon = (mon startx (starty * number) colar) >> callNTime (number -1) startx starty (cols ++ [c]) mon

 
initColor :: Display -> String -> IO Graphics.X11.Xlib.Pixel 
initColor dpy colour = do
  let colourmap = defaultColormap dpy (defaultScreen dpy)
  (apros, real) <- allocNamedColor dpy colourmap colour
  return $ color_pixel apros

printBits :: Word64 -> IO()
printBits word = print "adsadfalkdfdas dadad af" -- $ wordToBinary word --map (\ a -> if a then '1' else '0') (reverse $ wordToBinary word)

wordToBinary :: Word64 -> [Bool]
wordToBinary = work 63 
  where 
    work word n  
      | n >= 0 = if(word `div` (2 ^ n) > 0) then True:(work (word - (2^n)) (n-1)) else False:(work word (n-1))
      | otherwise = []

pixelFromRGB :: Word8 -> Word8 -> Word8 -> Word64 
pixelFromRGB r g b = ((shiftL ((shiftL (zeroBits .|. toWord64 r ) 8 ).|. toWord64 g) 8) .|. toWord64 b) 
  where 
    toWord64 :: Word8 -> Word64
    toWord64 = fromInteger . toInteger
--pixelFromRGB r g b = (fromIntegral (65025 * r) + fromIntegral (256 * g) + fromIntegral b) 
