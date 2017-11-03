module Main where
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
import Control.Concurrent (threadDelay)
import Data.Bits
type CS = Complex Scientific
type S = Scientific

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
  --r <- getLine 
  --i <- getLine
  --writeFile ("mand_txt/"++r++"+"++i++"i_mand.txt") $ show $ general_list_R mand_iteration ((read r :: Scientific) :+ (read i :: Scientific))
  --let list = [(makeSci (show x) "-1") :+ (makeSci (show y) "-1") | y <- [10,9..(-10)], x <- [-20..10]]
--  print $ length list
 -- print $ list !! 80000 
  --print $ mand $ (makeSci"1"  "-1") :+ (makeSci "1" "-1") 
  --mapM_ (\x -> appendFile "test777" $ ',':(show $ mand x)) list
--(writeFile "test777" $ ',':(show . mand)) list
  --writeFile "test666" $ show $ map mand list
  --let mand_list = map mand list
  --writePng ("benchmark.png") $ generateImage (\x y -> f 0.0005 (-2) (2) (2) x y) 8000 8000
  print $ cpow (0:+1) (0:+1)
  print "set for generating mandlebrot set, mov for the movement of the mandelbrot, grp for graphical, rang for range in a gif, sci for scientific use"
  whatdo <- getLine
  case whatdo of
    "set" -> generateSet
    "mov" -> generateMovement 
    "grp" -> startGraphical
    "rang" -> range
    "sci" -> generateSciSet
    "series" -> generateSeriesOfSets 
    "test" -> tester
    "acc" -> generateAccurateSet

generateAccurateSet = print "yet again not finishe"

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


startGraphical = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  let scr = defaultScreenOfDisplay dpy
  rootw <- rootWindow dpy dflt
  win <- mkUnmanagedWindow dpy scr rootw 0 0 100 100 
  name <- getLine
  setTextProperty dpy win name wM_NAME
  mapWindow dpy win
  sync dpy False
  threadDelay (10 * 1000000)
  exitWith ExitSuccess
  
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
    (nextl, nextOrig) = filter2 (map' mand_iteration original last) original removeLargeC 

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

generateSet = do
  print "enter step"
  step <- getLine
  print "enter starting real"
  sr <- getLine
  print "enter no of real points" 
  nr <- getLine
  print "starting im"
  si <- getLine
  print "no of im"
  ni <- getLine
  print "Enter name:"
  name <- getLine
  let stepf = read step :: Double
  let srf = read sr :: Double -- starting real
  let nrf = read nr :: Double --number of real points
  let sif = read si :: Double -- starting imaginary
  let nif = read ni :: Double -- number of imaginary points
  
  d <- getCurrentTime
  print "description:" 
  desc <- getLine
  appendFile "mand/log.txt" $ "\n"++(take 19 $ show d)++", "++name++", "++desc
    
  --writePng name $ generateImage (\x y -> PixelRGB8 (fromIntegral 0) (fromIntegral 0) (fromIntegral $ mand $ ((read sr :: S) + (read (show x) :: S) * (read step :: S)) :+ ((read si :: S) + (read (show y) :: S) * (read step :: S)))) (read nr :: Int) (read ni :: Int)
  writePng ("mand/"++name) $ generateImage (\x y -> f stepf srf sif (sif+ nif * stepf) x y ) (read nr :: Int) (read ni :: Int)
--writePng "mandv1" $ generateImage (\x y -> PixelRGB8 (fromIntegral 100) (fromIntegral 100) (fromIntegral 0)) 300 200
  print "completed"
  print "add comments:"
  comment <- getLine
  appendFile "mand/log.txt" $ ", "++comment
  



f stepf srf sif realend x y = fullcolour stepf srf sif realend x y
  where 
    m = mand $ Left $ (srf + (fromIntegral x) * stepf) :+ (realend - ((fromIntegral y) * stepf))
    normal stepf srf sif realend x y = PixelRGB8 (fromIntegral 0)  (fromIntegral 0) (fromIntegral m)
    fullcolour stepf srf sif realend x y = PixelRGB8 (scale $ fromIntegral $ max * max * (div m max* max)) (scale $ fromIntegral $ max * max * (mod (div m max) max * max)) $ ( scale $ max * max * (fromIntegral $ mod m max))
    max = 10
    scale x = round $ 255 * ((fromIntegral x) / (fromIntegral max))
    --m :: Int
    --m = mand $ Right $ fromFloatDigits (srf + (fromIntegral x) * stepf) :+ fromFloatDigits (sif + (fromIntegral y) * stepf)
    --m = mand $ ( (read :: String -> Scientific) (show (srf + (fromIntegral x) * stepf)) :=  (read :: String -> Scientific) (show (sif+ (fromIntegral y) * stepf))
    --m = if (mand $ (srf + (fromIntegral x) * stepf) :+ (sif + (fromIntegral y) * stepf) )== 255  then 0 else 255
   

--test = foldl (\acc x -> 'b':acc) []  [1..100]


colCorect1 :: Int -> Int
colCorect1 x = floor $ 256*(1 - exp(fromIntegral x/40))

colCorect2 :: Int -> Int
colCorect2 x = floor . (/255) . fromIntegral  $  x * x

makeSci :: String -> String -> Scientific
makeSci a b = read (a ++ "e" ++ b) :: Scientific

duplicate_args :: (a -> a -> b) -> a -> b
duplicate_args f a = f a a

csqrt :: (Floating a, RealFloat a) => Complex a -> Complex a
csqrt x = (m * cos (t) :+ m * sin(t))
  where
    p = polar x
    m = sqrt $ fst p
    t = (snd p)/2

mand_iteration :: RealFloat a => Complex a -> Complex a -> Complex a 
mand_iteration c z  = c + z*z 

mand_iterationA :: CS -> CS -> CS 
mand_iterationA (c:+b) (z:+x)  = (c + z*z - x* x) :+ (b + 2*z*x) 


cexp :: Floating a => Complex a -> Complex a
cexp (a :+ b) = (s * cos b :+ s * sin b)
  where 
    s = exp a

csin :: RealFloat a => Complex a -> Complex a
csin x = ((realPart c1 + realPart c2)/2 :+ (imagPart c1 + imagPart c2)/2)
 where
  c1 = exp x
  c2 = exp $ negate x 

cpow :: RealFloat a => Complex a -> Complex a -> Complex a
cpow x y = cexp $ y * log  x 
  
mand :: (RealFloat a) => Either (Complex a) CS -> Int
mand (Left c) = general mand_iteration c
mand (Right c) = generalA mand_iterationA c

--mand' :: CS -> Int
--mand' a = foldl (\(found, y, val)  x -> if found then (found, y, val) else ) 

sciMagnitude :: Complex Scientific  -> Scientific
sciMagnitude (a :+b) = a * a + b * b

general :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Complex a -> Int
general g a 
--  | ((realPart a) < 0.25) && ((realPart a) > (-0.5)) && (abs (imagPart a) < 0.5) = 255 takes 21 secods
  | (imagPart a == 0) || ( let b = a + (1:+0) in realPart(b*(conjugate b)) < 0.05) ||((realPart a < 0) && ( realPart (a * (conjugate a))  < 0.4)) ||(realPart a < 0.25 && realPart a >= 0 && abs (imagPart a) < 0.5) =  999 -- takes 16 seconds 
  | otherwise = count_iterations 0 (0 :+ 0) a 
  where 
    p = polar a
    count_iterations n e x 
      | n >= 999 = 999
      -- | n >= 255  = 255
--      | n >= 16581375 = 16581375 
      -- | (realPart x) < 0.1 && (realPart x) > -0.2 && abs (imagPart x) < 0.2 = 255 
      -- if using sci
      -- | (sciMagnitude x ) > (makeSci "4" "0") = n
      -- | otherwise = count_iterations (n + 1) e  $ g a $ roundComplex x 
      -- if using float
      | realPart (x * (conjugate x)) > 4 = n
      | otherwise = count_iterations (n +1) e $ g a x

generalA :: (CS -> CS -> CS) -> CS -> Int
generalA g a = count_iterations 0 (0 :+ 0) a 
  where 
    count_iterations n e x 
      -- | n >= 255  = 255
      | n >= 16581375 = 16581375 
      -- if using sci
      -- | (sciMagnitude x ) > (makeSci "4" "0") = n
      -- | otherwise = count_iterations (n + 1) e  $ g a $ roundComplex x 
      -- if using float
      | (realPart x)^2 + (imagPart x)^2 > 4 = n
      | otherwise = count_iterations (n +1) e $ g a $ roundComplex x


general_list_R ::  (Complex Scientific -> Complex Scientific -> Complex Scientific) -> Complex Scientific -> [Complex Scientific]
general_list_R g a = get_list 0 a (0 :+ 0) g
  where
    get_list 40 _ _ _ = []
    get_list n (c :+d) (e :+ f) g 
      | (sciMagnitude (e :+ f)) > (scientific 2 0) = []
      | otherwise = (e:+f):  (get_list (n +1) (c :+d) ( g (c :+ d) (roundComplex (e :+ f))) g) 

general_list ::  (Complex Scientific -> Complex Scientific -> Complex Scientific) -> Complex Scientific -> [Complex Scientific]
general_list g (a :+ b) = get_list 0  (a :+ b) (0 :+ 0) g
  where
    get_list 40  _ _ _ = []
    get_list n (c :+d) (e :+ f) g 
      | (sciMagnitude (e :+ f)) > (makeSci "2" "2") = []
      | otherwise = (e:+f):  (get_list (n +1) (c :+d) ( g (c :+ d) (e :+ f)) g) 

roundTo1000 :: Scientific -> Scientific
roundTo1000 s = makeSci (getSign:cut) getExp
  where 
    num = splitOn "e" $ show s
    removeSign = if s < 0 then tail (num !! 0) else (num !! 0)
    getSign = if s < 0 then '-'  else ' '
    getExp = if (length num) == 1 then "0" else num !! 1
    cut = take 20 removeSign

roundSci :: Scientific -> Scientific 
roundSci s 
  | d <  0  = s
  | otherwise = scientific (div (coefficient s) (10^d)) $ d + base10Exponent s
  where 
    d = (digitCount $ coefficient s) - 20 

digitCount :: Integer -> Int
digitCount = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds
--(fromInteger $ round $ f * (10^n)) / (10.0^^n)
--Integer 
roundComplex :: Complex Scientific -> Complex Scientific
roundComplex (a :+ b) = (roundSci a :+ roundSci b)
