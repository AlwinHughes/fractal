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
  img <- computeUnboxedP $ repaMandArray (-2) (-2) (0.0005) 8000 8000
  (savePngImage "./repatest1.png" . ImageRGB8 . toImage) img


repaMandArray :: Double -> Double -> Double -> Int -> Int -> Array D DIM2 (Word8, Word8, Word8)
repaMandArray start_re start_im step width height = fromFunction (Z :. width :. height) (arrayToMand start_re start_im step)

arrayToMand :: Double -> Double -> Double ->  Z :. Int :. Int -> (Word8, Word8, Word8) 
arrayToMand start_re start_im step arr@(Z :. x :. y) =  (0,0, fromIntegral $ general mand_iteration Nothing ((start_re + (fromIntegral x) * step ) :+(start_im + step * (fromIntegral y) ) ) 255)


toImage :: Array U DIM2 (Word8, Word8, Word8) -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b
