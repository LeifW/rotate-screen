{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Control.Concurrent.Lifted (threadDelay)
import Control.Monad (unless, forever)

import Control.Monad.IO.Class (MonadIO(), liftIO)

import Orientation (orientationFor, orientationToArg)
import UdevAccelerometer

import System.Process.Typed (runProcess_, proc)
import System.FilePath ((</>))

g :: Double -- m/s^2
g = 9.8
-- Just pick a number that you want to call close enough to laying flat that we should stop rotating the screen.
-- Tolerance
closeEnoughToG :: Double -- m/s^2
closeEnoughToG = 0.4

iio_device_name = "accel_3d"

printIO :: (MonadIO m, Show s) => s -> m () 
printIO = liftIO . print

run :: MonadIO m => FilePath -> [String] -> m ()
run cmd args = runProcess_ $ proc ("/usr/bin" </> cmd) args

xrandr, xinput :: MonadIO m => [String] -> m ()
xrandr = run "xrandr"
xinput = run "xinput"

display, touchscreen :: String
display = "eDP1"
touchscreen = "SYNA7813:00 06CB:1785"

udevMain ::  String -> String -> UdevIO ()
udevMain touchscreen display = do
    scale <- runOnDevice $ parseDouble <$> getAccelAttr "scale"
    forever $ do
      GravityVector x y z <- runOnDevice readOrientation
      unless (g + z * scale < closeEnoughToG) $ do
        printIO z
        let angle = atan2 y x
        printIO $ 180.0 / pi * angle
        printIO $ orientationToArg $ orientationFor angle
        xrandr ["-o", orientationToArg $ orientationFor angle]
        xinput ["map-to-output", touchscreen, display]
      threadDelay 1000000

main :: IO ()
main = runWithUDev iio_device_name $ udevMain touchscreen display
