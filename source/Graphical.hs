module Graphical where

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent 
import Data.Bits
import Data.Word
import Data.Scientific as Scientific
import Data.Complex

import Mand

startGraphical :: MVar Bool -> IO ()
startGraphical mv = do 
  print "graphical started"
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  let scr = defaultScreenOfDisplay dpy
  let background = whitePixel dpy dflt
  let border = blackPixel dpy dflt
  rootw <- rootWindow dpy dflt
  --win  <- createSimpleWindow dpy rootw 0 0 100 100 1 border background 
  win <- mkUnmanagedWindow dpy scr rootw 0 0 500 500 
  setTextProperty dpy win "this will not be seen" wM_NAME
  mapWindow dpy win
  --drawManyInWin 10 10 50 ["green","yellow", "blue","black"] dpy win 0 0 
  --callNTime 50 10 10 ["green","yellow", "blue","black"] (drawManyInWin 10 10 10 dpy win)
  --drawInWin dpy win "blue"
  col <- initColor dpy "red"
  --drawAPoint 400 400 dpy win col 
  drawSet dpy win (-1.5) (-0.05) 500 500 0.0002 mand_iteration 

  --drawRGBRec dpy win $ pixelFromRGB 150 200 25 
  --drawRGBRec dpy win $ pixelFromRGB 0 0 255
  sync dpy False
  return ()
  --threadDelay (10 * 1000000)
  --exitWith ExitSuccess
  
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

drawSet :: RealFloat a => Display -> Window -> a -> a -> Int -> Int -> a -> (Complex a -> Complex a -> Complex a) -> IO ()
drawSet dpy win startRe startIm numberRe numberIm step nextIteration = do 
  gc <- createGC dpy win
  mapM_ (stuff gc ) [(x,y) | x <- [0..(numberRe-1)] , y <- [0..(numberIm -1)] ] 
  freeGC dpy gc
    where
      --stuff ::  -> (Int, Int) -> IO ()
      stuff gc (x, y) = do 
        let re = startRe + (fromIntegral x) * step 
        let im = startIm + (fromIntegral y) * step
        let pixel = scaleIntToRGB (general nextIteration (re :+ im) 2197) 13 
        setForeground dpy gc pixel 
        drawPoint dpy win gc (fromIntegral x) (fromIntegral y)
       
--the peramiter max should be the third root of the maximum value that n can take
--scaleIntToRGB n max = if n == max * max * max then 0 else pixelFromRGB c1 c2 c3
scaleIntToRGB :: Int -> Int -> Word64
scaleIntToRGB n max = pixelFromRGB c1 c2 c3
  where
    scale x = round $ 255 * ((fromIntegral x) / fromIntegral (max))
    c1 = scale $ div n (max* max)
    c2 = scale $ mod (div n max) (max * max)
    c3 = scale $ mod n max
    
    


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
  fillRectangle dpy win gc 100 100 100 100 
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

pixelFromRGB :: Word8 -> Word8 -> Word8 -> Graphics.X11.Xlib.Pixel 
pixelFromRGB r g b = ((shiftL ((shiftL (zeroBits .|. toWord64 r ) 8 ).|. toWord64 g) 8) .|. toWord64 b) 
  where 
    toWord64 :: Word8 -> Word64
    toWord64 = fromInteger . toInteger
--pixelFromRGB r g b = (fromIntegral (65025 * r) + fromIntegral (256 * g) + fromIntegral b) 
