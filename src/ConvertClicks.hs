-- Usage: xmacrorec2 | ConvertClicks > clicks.txt

module Main (main) where
import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let printFunc = toClick $ if "--haskell" `elem` args
                                  then printTuple
                                  else printCommand
    void $ mainLoop printFunc ("", "")
    where xOffset = id
          yOffset = flip (-) 38
          printTuple x y = print (xOffset x, yOffset y)
          printCommand x y = putStrLn $ "click " ++ show (xOffset x) ++ " "
                             ++ show (yOffset y)


mainLoop :: PrintFunc -> (String, String) -> IO (String, String)
mainLoop prt window = getLine >>= updateWindow >>= prt >>= mainLoop prt
    where move (_, a) b = (a, b)
          updateWindow = return . move window

-- |Output click with coordinates if applicable.
-- >>> toClick (\x y -> print (x, y)) ("MotionNotify 1500 550","ButtonPress 1")
-- (1500,550)
-- ("MotionNotify 1500 550","ButtonPress 1")
-- >>> toClick (\x y -> print (x, y)) ("ButtonPress 1","MotionNotify 1500 550")
-- ("ButtonPress 1","MotionNotify 1500 550")
toClick :: (Int -> Int -> IO ()) -> (String, String) -> IO (String, String)
toClick prt window@(a, b)
    | isClick b = print' (toXY a) >> return window
    | otherwise = return window
    where isClick = (==) "ButtonPress 1"
          print' Nothing       = return ()
          print' (Just (x, y)) = prt x y


-- |Return the XY coordinates from the string.
-- >>> toXY "MotionNotify 123 456"
-- Just (123,456)
-- >>> toXY "Bla 123 456"
-- Nothing
toXY :: String -> Maybe (Int, Int)
toXY a
    | "MotionNotify " `isPrefixOf` a = Just (x, y)
    | otherwise                      = Nothing
    where xy = dropWhile (not . isDigit) a
          x  = read $ takeWhile isDigit xy
          y  = read $ dropWhile isDigit xy

type PrintFunc = (String, String) -> IO (String, String)
