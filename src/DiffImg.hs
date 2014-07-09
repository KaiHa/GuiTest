module Main where
import Codec.Picture
import Codec.Picture.Types   (promoteImage, ColorConvertible(..))
import Control.Applicative   ((<$>))
import Control.Monad         (when)
import Data.List             (isSuffixOf)
import System.Directory      (doesFileExist, getDirectoryContents)
import System.Environment    (getArgs)
import System.Exit           (exitFailure)
import System.FilePath.Posix (takeBaseName, (</>))

-- |Compare the content of two directories. Only .png images are compared.
main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) printUsage
    getDirectoryContents (args !! 0)
        >>= dirDiff (args !! 0) (args !! 1)
        >>= flip when exitFailure
    where printUsage = putStrLn "usage: DiffImg DIRECTORY_A DIRECTORY_B" >> exitFailure

-- |Compare a list of images that have one exemplar in both directories.
dirDiff :: FilePath -> FilePath -> [FilePath] -> IO Bool
dirDiff dirA dirB files = or <$> mapM diff pngs
    where diff file = fileDiff (dirA </> file) (dirB </> file)
          pngs = [x | x <- files, ".png" `isSuffixOf` x]



-- |Compare two image files. See 'imageDiff' below.
fileDiff :: FilePath -> FilePath -> IO Bool
fileDiff fileA fileB = do
    missing <- not <$> doesFileExist fileB
    if missing
        then printMissing >> return True
        else do
            a <- readPng fileA
            b <- readPng fileB
            let (hasDiff, result) = diff a b
            write outputFile result
            when hasDiff $ putStrLn diffMsg
            return hasDiff
    where diff (Right a) (Right b) = dynImageDiff a b
          diff (Left a)         _  = error a
          diff _         (Left b)  = error b
          diffMsg      = fileA ++ " and " ++ fileB ++ " differ!"
          printMissing = putStrLn (fileB ++ " is missing!")
          outputFile   = (takeBaseName fileA) ++ ".diff.png"
          write fname (Right png) =  writePng fname png
          write _     (Left err)  = putStrLn $ diffMsg ++ " " ++ err

-- |Compare two 'DynamicImage'. See 'imageDiff' below.
dynImageDiff :: DynamicImage -> DynamicImage -> (Bool, Either String (Image PixelRGB8))
dynImageDiff (ImageRGB8   a) (ImageRGB8   b) = imageDiff (promoteImage a) (promoteImage b)
dynImageDiff (ImageRGB8   a) (ImageRGBA8  b) = imageDiff (promoteImage a) (promoteImage b)
dynImageDiff (ImageRGBA8  a) (ImageRGB8   b) = imageDiff (promoteImage a) (promoteImage b)
dynImageDiff (ImageRGBA8  a) (ImageRGBA8  b) = imageDiff (promoteImage a) (promoteImage b)
dynImageDiff (ImageRGB16  a) (ImageRGB16  b) = imageDiff (promoteImage a) (promoteImage b)
dynImageDiff (ImageRGB16  a) (ImageRGBA16 b) = imageDiff (promoteImage a) b
dynImageDiff (ImageRGBA16 a) (ImageRGB16  b) = imageDiff a                (promoteImage b)
dynImageDiff (ImageRGBA16 a) (ImageRGBA16 b) = imageDiff a                b
dynImageDiff _              _              = error "Only images with 8 or 16-bit/color RGB(A) are supported"

p8to16a :: Image PixelRGB8 -> Image PixelRGBA16
p8to16a = promoteImage

-- |Compare two images. The returned 'Bool' is True when the two images differ.
-- The returned 'Image' contains the differences. Differnces are shown as white
-- pixels. Equal pixels are shown as black. Ignored areas are shown as green.
-- Areas that are 100% transparent in the first image are ignored.
imageDiff :: Image PixelRGBA16 -> Image PixelRGBA16 -> (Bool, Either String (Image PixelRGB8))
imageDiff a@(Image a_width a_height _) b@(Image b_width b_height _)
    | a_width  /= b_width  = (False, Left "Images have different width.")
    | a_height /= b_height = (False, Left "Images have different height.")
    | otherwise = right $ generateFoldImage pixelDiff False a_width a_height
    where pixelDiff acc x y =
              if ignorePixel
              then ign
              else if pixelAt a x y == pixelAt b x y
                   then ok
                   else nok
              where ignorePixel = pixelOpacity (pixelAt a x y) == 0
                    ok  = (acc,  PixelRGB8 0     0   0)
                    ign = (acc,  PixelRGB8 0   150   0)
                    nok = (True, PixelRGB8 255 255 255)
          right (c,d) = (c, Right d)
