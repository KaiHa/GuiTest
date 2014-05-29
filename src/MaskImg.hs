module Main (main) where
import Codec.Picture
import Codec.Picture.Types
import Control.Monad       (when)
import System.Environment  (getArgs)
import System.Exit         (exitFailure)

-- |Copy the transparent areas from mask.png into input.png and write the result
-- to output.png.
main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) printUsage
    mask  <- readPng $ args !! 0
    input <- readPng $ args !! 1
    let ignore = isTransparent mask
    writePng (args !! 2) $ makeTransparent ignore input
    where printUsage = putStrLn "usage: MaskImg MASK.PNG INPUT.PNG OUTPUT.PNG" >> exitFailure


makeTransparent :: (Int -> Int -> Bool) -> Either String DynamicImage -> Image PixelRGBA16
makeTransparent ignore (Right (ImageRGB16 img@(Image w h _))) =
    generateImage copy w h
    where copy :: Int -> Int -> PixelRGBA16
          copy x y
            | ignore x y = PixelRGBA16 0 0 0 0
            | otherwise  = promotePixel $ pixelAt img x y
makeTransparent _ (Left a) = error a
makeTransparent _ _        = error "Only images with 16-bit/color RGB are supported."


-- |Returns True if the pixel at the position Int Int in the given image is
-- transparent.
isTransparent :: Either String DynamicImage -> Int -> Int -> Bool
isTransparent (Right (ImageRGBA16 img)) x y = (pixelOpacity $ pixelAt img x y) == 0
isTransparent (Right (ImageRGBA8  img)) x y = (pixelOpacity $ pixelAt img x y) == 0
isTransparent (Left a)                  _ _ = error a
isTransparent _                         _ _ = error "Only images with 8 or 16-bit/color RGBA supported."
