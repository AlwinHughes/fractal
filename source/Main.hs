module Main where
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

import Graphical
import Mand

--instance RealFloat Scientific where
  --floatRadix a = 1
  --floatDigits s = 1
  --floatRange s = (10000, 10000)
  --decodeFloat s = (coefficient s, base10Exponent s)
  --encodeFloat c e = scientific c e
  --exponent s = base10Exponent s
  --significand a = a
  --scaleFloat a b = b
  --isNaN _ = False
  --isInfinite _ = False
  --isDenormalized _ = False
  --isNegativeZero _ = False
  --isIEEE _ = False
  --atan2 s _ = s 


main :: IO ()
main = do 
  taking_input <- newMVar True  
  forever $ loop taking_input

loop taking_input = do
  --writePng ("benchmark.png") $ generateImage (\x y -> f 0.0005 (-2) (2) (2) x y) 8000 8000
  --print $ cpow (0:+1) (0:+1)
  input <- promptForImput taking_input "set for generating mandlebrot set, mov for the movement of the mandelbrot, grp for graphical, rang for range in a gif, sci for scientific use"
  case input of
    "set" -> generateSet taking_input 
    "mov" -> generateMovement 
    "grp" -> forkIO (startGraphical taking_input)>> return ()
    "rang" -> range
    "sci" -> generateSciSet
    "series" -> generateSeriesOfSets 
    "test" -> tester
    "acc" -> generateAccurateSet

generateAccurateSet = print "yet again not finishe"

promptForImput :: MVar Bool -> String ->  IO String
promptForImput mv message = do
  takeMVar mv
  print message
  input <- getLine  
  putMVar mv True
  if input == "*" then promptForImput mv message else return input

tester = do
  writePng ("benchmark.png") $ generateImage (\x y -> f 0.0005 (-2) (2) (2) x y) 8000 8000
--stepf srf sif realend x y
generateSeriesOfSets = print "in complete"

generateSciSet = do 
  print "nothign yet"
-- print $ mi (makeSciComplex "1e-1" "2e-1") $ makeSciComplex "-9e-1" "4e-2" 

--mi :: CS -> CS -> CS 
--mi c z = c + z*z

makeSciComplex :: String -> String -> CS
makeSciComplex x y = (read x :: S) :+ (read x :: S)

range = do
  print "in complete"  

generateMovement = do
  let list = [fromIntegral x/10:+ fromIntegral y/10 |x <-[(-400)..399], y <- [(-400)..399]]
  let zlist = foldl (\acc x -> (0:+0):acc) [] [0..639999] 
  let list' = map' mand_iteration list list 
  --print list
  --writePng "movv5_i1.png" $ generateImage (genMovImg $ map roundComplex2 list) 399 399
  --writePng "movv5_i2.png" $ generateImage (genMovImg $ map roundComplex2 list') 399 399
  --let list'1 = filterC $ map' mand_iteration list list' 
  --writePng "movv5_i3.png" $ generateImage (genMovImg $ map roundComplex2 list'1) 399 399
  --let list'2 = filterC $ map' mand_iteration list list'1 
  --writePng "movv5_i4.png" $ generateImage (genMovImg $ map roundComplex2 list'2) 399 399
  --let list'3 = filterC $ map' mand_iteration list list'2 
  --writePng "movv5_i5.png" $ generateImage (genMovImg $ map roundComplex2 list'3) 399 399
  --let list'4 = filterC $ map' mand_iteration list list'3
  
  mapM_ (curry' writePng) $ reverse $ snd $ mapAccumR nextMov (list, zlist) [0..10]
  print "complete"

curry' :: (a -> b -> c) -> (a, b) ->  c
curry' f (a,b) = f a b

nextMov (last, original) x = ((nextl, nextOrig), ("movv10_i"++(show x)++".png",generateImage (genMovImg $ map roundComplex2 nextl) 3999 3999))
  where 
    (nextl, nextOrig) = filter2 (map' mand_iteration original last ) original removeLargeC 

removeLargeC :: (Num a, Ord a )=> Complex a -> Bool
removeLargeC (a :+ b) = if abs a >2 || abs b > 2 then False else True

filterC = filter removeLargeC 

filter2 :: [a] -> [b] -> (a -> Bool) -> ([a],[b])
filter2 x y f = (fst ziped,snd ziped)
  where 
    ziped = foldr (\(a,b) (ac,bc) -> if f a then (a:ac,b:bc) else (ac,bc)) ([],[]) $ zip x y

genMovImg arr x y = if elem (fromIntegral (x-2000) /1000 :+ fromIntegral (y-2000)/1000) arr then PixelRGB8 0 0 0 else PixelRGB8 255 255 255 

map' :: (a -> a -> b) -> [a] -> [a] -> [b]
map' _ [] [] = []
map' f (x:xs) (y:ys) = (f x y):(map' f  xs ys)

g :: [Bool]
g = foldr (\x acc -> if mod x 10 == 0 then True:acc else False:acc) [] [0..399]

h :: [[Bool]] 
h = foldr (\x acc -> if mod x 10 == 0 then g:acc else i:acc) [] [0..399]

i :: [Bool]
i = foldr (\_ acc -> False:acc) [] [0..399]

falseArr = foldr (\_ acc -> i:acc) [] [0..399]

toComplex :: RealFloat a => [[Bool]] -> [Complex a]
toComplex arr = foldr (\x acc1 -> (foldr (\y acc2 -> if arr !! x !! y then (fromIntegral (x-200)/100 :+ fromIntegral (y-200)/100):acc2 else acc2) [] [0..((length $ arr !! 0)-1)])++acc1) [] [0..((length arr) -1)] 

toArr :: RealFloat a => [Complex a] -> [[Bool]]
toArr arr = f arr falseArr
  where 
    rarr = map roundComplex2 arr 
    f :: RealFloat a=> [Complex a] -> [[Bool]]-> [[Bool]]
    f [] fa  = fa
    f (x:xs) fa = f xs $ modifyarr fa  newx newy 
      where
        newx = round $ 1000*(realPart x) +2000
        newy = round $ 1000*(imagPart x) +2000

modifyarr :: [[Bool]] -> Int -> Int -> [[Bool]]
modifyarr arr x y = foldr (\a acc -> (foldr (\b acc2 -> if x == a && b == y then True:acc2 else (arr !! x !! y):acc2) [] [0..((length $ arr !! 0)-1)]):acc) [] [0..((length arr)-1)]

roundComplex2 :: RealFloat a => Complex a -> Complex a
roundComplex2 (a :+ b) = (fromIntegral (round $ a*1000)/1000 :+ fromIntegral (round $ b*1000)/1000)

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
  writePng ("mand/"++name) $ generateImage (\x y -> f stepf srf sif (sif+ nif * stepf) x y ) (read nr :: Int) (read ni :: Int)
--writePng "mandv1" $ generateImage (\x y -> PixelRGB8 (fromIntegral 100) (fromIntegral 100) (fromIntegral 0)) 300 200
  print "completed"
  comment <- promptForImput mv "add comments:"
  appendFile "mand/log.txt" $ ", "++comment
  



f stepf srf sif realend x y = fullcolour stepf srf sif realend x y
  where 
    m = general cubed_iteration (Just zzcosMask) ((srf + (fromIntegral x) * stepf) :+ (realend - ((fromIntegral y) * stepf))) 5000 
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
