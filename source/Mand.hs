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

--cubed_iteration c z  = c + z * cos (z ** z) 
cubed_iteration :: RealFloat a => Complex a -> Complex a -> Complex a 
cubed_iteration c z = c + z * z * z

new_iteration :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a 
new_iteration c n_z o_z = c + n_z * o_z 

zzcosMask :: RealFloat a => Complex a -> Bool
zzcosMask (x :+y) = ((x > -0.95 && x < 0.1) && (y < 0.47 && y > -0.47)) || (( x <= -0.95 && x > -1.25) && (abs y < 0.42)) || (abs y < 0.2 && x <= -1.25 && x > -1.45) || (abs y < 0.4 && x >= 0.1 && x < 0.2) || (abs y < 0.17 && (x > 0.6 && x < 0.78)) || (abs y < 0.11 && x > 0.4 && x <= 0.85)

zzcos_iteration :: RealFloat a => Complex a -> Complex a -> Complex a 
zzcos_iteration c z = c + z * z * (cos z)

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
mand (Left c) max = general mand_iteration (Just mandSive) c max
mand (Right c) _ = generalA mand_iterationA c

sciMagnitude :: Complex Scientific  -> Scientific
sciMagnitude (a :+b) = a * a + b * b

mandSive :: RealFloat a => Complex a -> Bool
mandSive c@(x:+y) = ( let b = c + (1:+0) in realPart(b*(conjugate b)) < 0.05) ||((x < 0) && ( realPart (c * (conjugate c))  < 0.4)) ||(x < 0.25 && x  >= 0 && abs y  < 0.5)  -- takes 16 seconds 

--( let b = a + (1:+0) in realPart(b*(conjugate b)) < 0.05) ||((realPart a < 0) && ( realPart (a * (conjugate a))  < 0.4)) ||(realPart a < 0.25 && realPart a >= 0 && abs (imagPart a) < 0.5) =  max -1  -- takes 16 seconds 

general :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Maybe (Complex a -> Bool)  -> Complex a -> Int -> Int
general g Nothing a max = count_iterations 0 (0 :+ 0) a
  where 
    count_iterations n e x 
      | n == max = max -1
      | realPart (x * (conjugate x)) > 4 = n
      | otherwise = count_iterations (n +1) e $ g a x
general g (Just sive) a max 
--  | ((realPart a) < 0.25) && ((realPart a) > (-0.5)) && (abs (imagPart a) < 0.5) = 255 takes 21 secods
-- removed real check: (imagPart a == 0 && realPart a < 0.25 && realPart a > -2) || 
  | sive a = max - 1 
  | otherwise = count_iterations 0 (0 :+ 0) a 
  where 
    count_iterations n e x 
      | n == max = max -1
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
general_list_R g a = get_list 0 a (0 :+ 0)
  where
    get_list 1000 _ _  = []
    get_list n (c :+d) (e :+ f)  
      | (sciMagnitude (e :+ f)) > (scientific 2 0) = []
      | otherwise = (e:+f):  (get_list (n +1) (c :+d) ( g (c :+ d) (roundComplex (e :+ f)))) 
-- takes 1:13 to do 1000 iterations using roundComplex

general_list ::  (Complex Scientific -> Complex Scientific -> Complex Scientific) -> Complex Scientific -> [Complex Scientific]
general_list g (a :+ b) = get_list 0  (a :+ b) (0 :+ 0)
  where
    get_list 40  _ _  = []
    get_list n (c :+d) (e :+ f) 
      | (sciMagnitude (e :+ f)) > (makeSci "2" "2") = []
      | otherwise = (e:+f):  (get_list (n +1) (c :+d) ( g (c :+ d) (e :+ f))) 

general_list_D :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Complex a -> [Complex a]
general_list_D g x = getlist 0 x (0 :+ 0)
  where
    getlist 1000 _ _ = []
    getlist n c z 
      | realPart (z * conjugate z)> 4 = []
      | otherwise = z: (getlist (n +1) c (g c z)) 

roundSci' :: Scientific -> Scientific 
roundSci' s = makeSci (getSign:cut) getExp
  where 
    num = splitOn "e" $ show s
    removeSign = if s < 0 then tail (num !! 0) else (num !! 0)
    getSign = if s < 0 then '-'  else ' '
    getExp = if (length num) == 1 then "0" else num !! 1
    cut = take 100  removeSign

--rounds t 20 dp
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
roundComplex (a :+ b) = (roundSci' a :+ roundSci' b)
