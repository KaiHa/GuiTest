module Main (main) where

import Control.Applicative       ((<$>))
import Control.Concurrent        (threadDelay)
import Graphics.X11.Types        (button1, Window)
import Graphics.X11.Xlib.Display (defaultScreen, openDisplay, rootWindow)
import Graphics.X11.Xlib.Misc    (queryPointer)
import Graphics.X11.Xlib.Types   (Display, ScreenNumber)
import Graphics.X11.XTest        (fakeButtonPress, fakeMotion)
import System.Environment        (getEnvironment)
import System.Process            (runProcess, system, terminateProcess
                                 , waitForProcess, ProcessHandle)

main :: IO ()
main = do
    xvfb <- startVirtFrambuffer
    visrun <- startVisrun
    display <- openDisplay ":1"
    let screen = defaultScreen display
    rootw <- rootWindow display screen
    let click' = click display rootw screen
    screenshot 0
    getRecord >>= (mapM_ click') . zip [1..]
    kill visrun
    kill xvfb

click :: Display -> Window -> ScreenNumber -> (Int, (Int, Int)) -> IO ()
click display window screen (num, (x, y)) =
    fakeMotion display screen x y
    >> fakeButtonPress display button1
    >> queryPointer display window -- HACK
    >> sleep 1
    >> screenshot num

-- |Make a screenshot.
screenshot :: Int -> IO ()
screenshot num =
    system ("DISPLAY=:1 import -window root actual/screenshot_" ++ show num ++ ".png")
    >> return ()

-- |Read the list of coordinates to click on.
getRecord :: IO [(Int, Int)]
getRecord = map read <$> lines <$> getContents

startVirtFrambuffer :: IO ProcessHandle
startVirtFrambuffer = do
    h <- run "Xvfb" [":1", "-screen", "0", "1280x1024x24"] Nothing
    sleep 1
    return h

startVisrun :: IO ProcessHandle
startVisrun = do
    env <- getEnvironment
    h <- run "visrun" ["-alaheight", "0", "-geom", "1280x1024"]
         $ Just $ [("DISPLAY", ":1")] ++ env
    sleep 4
    return h

sleep :: Int -> IO ()
sleep = threadDelay . (*) 1000000

run :: FilePath -> [String] -> Maybe [(String, String)] -> IO ProcessHandle
run a b c = runProcess a b Nothing c Nothing Nothing Nothing

kill :: ProcessHandle -> IO ()
kill a = terminateProcess a >> waitForProcess a >> return ()
