module Mand where
import Data.List.Split
import Data.Scientific as Scientific
import Data.Complex

type CS = Complex Scientific
type S = Scientific

makeSci :: String -> String -> Scientific
makeSci a b = read (a ++ "e" ++ b) :: Scientific

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
  
mand :: (RealFloat a) => Either (Complex a) CS -> Int -> Int
mand (Left c) max = general mand_iteration c max
mand (Right c) _ = generalA mand_iterationA c

sciMagnitude :: Complex Scientific  -> Scientific
sciMagnitude (a :+b) = a * a + b * b

general :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Complex a -> Int -> Int
general g a max 
--  | ((realPart a) < 0.25) && ((realPart a) > (-0.5)) && (abs (imagPart a) < 0.5) = 255 takes 21 secods
-- removed real check: (imagPart a == 0 && realPart a < 0.25 && realPart a > -2) || 
  | ( let b = a + (1:+0) in realPart(b*(conjugate b)) < 0.05) ||((realPart a < 0) && ( realPart (a * (conjugate a))  < 0.4)) ||(realPart a < 0.25 && realPart a >= 0 && abs (imagPart a) < 0.5) =  max -1  -- takes 16 seconds 
  | otherwise = count_iterations 0 (0 :+ 0) a 
  where 
    count_iterations n e x 
      | n == max = max -1
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
