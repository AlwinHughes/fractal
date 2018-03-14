module GenerateSet where 

import Data.Word
import Data.List.Split
import Data.Scientific as Scientific
import Data.Complex
import Control.Monad
import Codec.Picture
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent 
import Data.Bits
import GHC.Conc (numCapabilities)

import Mand


generateSet mv = do
  step <- promptForImput mv "enter step" 
  sr <- promptForImput mv "enter starting real"
  nr <- promptForImput mv "enter no of real points" 
  si <- promptForImput mv "starting im"
  ni <- promptForImput mv "no of im"
  name <- promptForImput mv "Enter name:"

  let stepf = read step :: Double
  let srf = read sr :: Double -- starting real
  let nrf = read nr :: Double --number of real points
  let sif = read si :: Double -- starting imaginary
  let nif = read ni :: Double -- number of imaginary points
  
  d <- getCurrentTime
  desc <- promptForImput mv "description:" 
  appendFile "mand/log.txt" $ "\n"++(take 19 $ show d)++", "++name++", "++desc
    
  --writePng name $ generateImage (\x y -> PixelRGB8 (fromIntegral 0) (fromIntegral 0) (fromIntegral $ mand $ ((read sr :: S) + (read (show x) :: S) * (read step :: S)) :+ ((read si :: S) + (read (show y) :: S) * (read step :: S)))) (read nr :: Int) (read ni :: Int)
  writePng ("mand/"++name) $ generateImage (\x y -> generatingFunction stepf srf sif (sif+ nif * stepf) x y ) (read nr :: Int) (read ni :: Int)
--writePng "mandv1" $ generateImage (\x y -> PixelRGB8 (fromIntegral 100) (fromIntegral 100) (fromIntegral 0)) 300 200
  print "completed"
  comment <- promptForImput mv "add comments:"
  appendFile "mand/log.txt" $ ", "++comment
  



generatingFunction stepf srf sif realend x y = normal stepf srf sif realend x y
  where 
    m = general mand_iteration Nothing ((srf + (fromIntegral x) * stepf) :+ (realend - ((fromIntegral y) * stepf))) 255 
    normal stepf srf sif realend x y = PixelRGB8 (fromIntegral 0)  (fromIntegral 0) (fromIntegral m)
    fullcolour stepf srf sif realend x y = PixelRGB8 p1 p2 p3
    max = 10
    scale x = round $ 255 * ((fromIntegral x) / (fromIntegral (max * max * max -1)))
    --p1 = scale $ fromIntegral $ max * max * (div m max* max)
    --p2 = scale $ fromIntegral $ max * max * (mod (div m max) max * max)
    --p3 = scale $ max * max * (fromIntegral $ mod m max)
    p1 = fromIntegral $ div (scaleUp 5000 m) 65536
    p2 = fromIntegral $ mod (div (scaleUp 5000 m) 255) 256 
    p3 = fromIntegral $ mod (scaleUp 5000 m) 255 


scaleUp :: Integral a => a -> a -> a 
scaleUp max i = round $ 16777215 * fromIntegral i / fromIntegral max
    --m :: Int
    --m = mand $ Right $ fromFloatDigits (srf + (fromIntegral x) * stepf) :+ fromFloatDigits (sif + (fromIntegral y) * stepf)
    --m = mand $ ( (read :: String -> Scientific) (show (srf + (fromIntegral x) * stepf)) :=  (read :: String -> Scientific) (show (sif+ (fromIntegral y) * stepf))
    --m = if (mand $ (srf + (fromIntegral x) * stepf) :+ (sif + (fromIntegral y) * stepf) )== 255  then 0 else 255
   

--test = foldl (\acc x -> 'b':acc) []  [1..100]


colCorect1 :: Int -> Int
colCorect1 x = floor $ 256*(1 - exp(fromIntegral x/40))

colCorect2 :: Int -> Int
colCorect2 x = floor . (/255) . fromIntegral  $  x * x

promptForImput :: MVar Bool -> String ->  IO String
promptForImput mv message = do
  takeMVar mv
  print message
  input <- getLine  
  putMVar mv True
  if input == "*" then promptForImput mv message else return input
