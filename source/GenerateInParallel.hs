{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module GenerateInParallel where


import Data.Complex
import Control.Monad
import Codec.Picture
import Data.Array.Repa 
import Data.Word

import Mand


generateInParallel = do
  mapM_ (\ x-> (computeUnboxedP $ repaMandArray x (-2) (-2) (0.0025) 1600 1600) >>= (savePngImage ("./repatestcol" Prelude.++ show x Prelude.++ ".png") . ImageRGB8 . toImage)  ) [2..3]


  --img <- computeUnboxedP $ repaMandArray (-2) (-2) (0.0005) 8000 8000
  --(savePngImage "./repatest1.png" . ImageRGB8 . toImage) img


repaMandArray :: Int -> Double -> Double -> Double -> Int -> Int -> Array D DIM2 Word16
repaMandArray pow start_re start_im step width height = fromFunction (Z :. width :. height) (arrayToMand pow start_re start_im step)

arrayToMand :: Int -> Double -> Double -> Double ->  Z :. Int :. Int -> Word16
arrayToMand pow start_re start_im step (Z :. x :. y) =  fromIntegral $ general (mand_pow_iteration pow) Nothing ((start_re + (fromIntegral x) * step ) :+(start_im + step * (fromIntegral y) ) ) 32767


toImage :: Array U DIM2 Word16 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = extent a
    gen x y =
      let d = a ! (Z :. x :. y)
      in PixelRGB8 (fromIntegral $ 2 * mod d 128) (fromIntegral $ 2* mod (div d 128) 128) ( fromIntegral $ 2 * div d 16384)  
