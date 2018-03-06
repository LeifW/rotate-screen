{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Control.Concurrent.Lifted (threadDelay)
import Control.Monad (unless, forever)

import Control.Monad.IO.Class (MonadIO(), liftIO)

import Orientation (orientationFor, orientationToArg)
import UdevAccelerometer

import Data.Metrology
import Data.Metrology.SI

import Data.Constants.Mechanics (gravity_g)

import Data.Metrology.Show ()

import System.Process.Typed (runProcess_, proc)
import System.FilePath ((</>))

scaledBy :: Double -> Double -> Acceleration
scaledBy scale i = i *| (scale % [si| m/s^2 |])

-- Just pick a number that you want to call close enough to laying flat that we should stop rotating the screen.
-- Tolerance
closeEnoughToG :: Acceleration
closeEnoughToG = 0.4 % [si| m/s^2 |]

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

udevMain :: SysValue -> String -> String -> UdevIO ()
udevMain iio_device_name touchscreen display = do
    path <- getDevPath iio_device_name
    scale <- runOnDevice path $ parseDouble <$> getAccelAttr "scale"
    let scaled = scaledBy scale
    forever $ do
      GravityVector x y z <- fmap scaled <$> runOnDevice path readOrientation
      unless (gravity_g |+| z < closeEnoughToG) $ do
        printIO z
        --let angle = atan2 y x
        let angle = atan $ y |/| x # Number
        printIO $ 180.0 / pi * angle
        printIO $ orientationToArg $ orientationFor angle
        xrandr ["-o", orientationToArg $ orientationFor angle]
        xinput ["map-to-output", touchscreen, display]
      threadDelay 1000000

main :: IO ()
main = runWithUDev $ udevMain iio_device_name touchscreen display
