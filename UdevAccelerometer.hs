{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, QuasiQuotes #-}
module UdevAccelerometer where

import Data.ByteString (ByteString) 
import qualified Data.ByteString as BS

import Data.Text (Text())
import Data.Text.Encoding as E
import Data.Text.Read (decimal, double)

import System.UDev.Context
import System.UDev.Device
import System.UDev.Enumerate
import System.UDev.List

import System.Posix.ByteString.FilePath (RawFilePath())

import Data.Word (Word16())
import Data.Bits (testBit)

import Control.Concurrent(threadDelay)
import Control.Monad (unless, forever)

import Control.Applicative (liftA2)
import Data.Function (on)

import Control.Monad.Reader

import Orientation (orientationFor, orientationToArg)
--import Units

import Data.Metrology
import Data.Metrology.SI

import Data.Constants.Mechanics

import Data.Metrology.Show ()

import System.Process.Typed (runProcess_, proc)
import System.FilePath ((</>))

type DeviceIO = ReaderT Device IO
type UdevIO = ReaderT UDev IO

twos_complement :: Word16 -> Int
twos_complement bits = if isNeg
  then -- negate the bits, turn it into an Int, then make it a negative Int
    negate $ fromIntegral $ negate bits 
  else
    fromIntegral bits
  where
    isNeg = testBit bits 15

data GravityVector = GravityVector {
  x :: Acceleration,
  y :: Acceleration,
  z :: Acceleration
}

accelAttr :: ByteString -> Device -> IO ByteString
accelAttr name dev = getSysattrValue dev (BS.append "in_accel_" name)

getAccelAttr :: ByteString -> DeviceIO  ByteString
getAccelAttr name = ask >>= lift . accelAttr name

-- Everything that consumes these wants a Double, so I'll go ahead and convert the Int to a Double in here.
getDimension :: ByteString -> DeviceIO Double
getDimension name = (fromIntegral . parseInt) <$> getAccelAttr (BS.append name "_raw")
--getDimension dev v = getSysattrValue dev $ BS.concat ["in_accel_", v, "_raw"]


parseInt :: ByteString -> Int
parseInt = twos_complement . fromReader . decimal . E.decodeUtf8

parseDouble :: ByteString -> Double
parseDouble = fromReader . double . E.decodeUtf8

--parseInt bs = n
--  where Right (n, "") = decimal $ E.decodeUtf8 bs

--partial
fromReader :: Either String (a, Text) -> a
fromReader (Right (n, "")) = n

--main :: IO ()
--main = forever $ foo *> threadDelay 1000000

--g :: Double
--g = 9.8 -- m/s^2

scaledBy :: Double -> Double -> Acceleration
scaledBy scale i = i *| (scale % [si| m/s^2 |])

-- Just pick a number that you want to call close enough to laying flat that we should stop rotating the screen.
-- Tolerance
closeEnoughToG :: Acceleration
closeEnoughToG = 0.4 % [si| m/s^2 |]

iio_device_name = "accel_3d"
--    addMatchSysattr e "name" (Just "accel_3d")
-- 
getDevPath:: SysValue -> UdevIO RawFilePath
getDevPath devName = ask >>= lift . devPath devName

devPath :: SysValue -> UDev -> IO RawFilePath
devPath devName udev = do
    e <- newEnumerate udev
    addMatchSubsystem e "iio"
    addMatchSysattr e "name" (Just devName)
    scanDevices e
    Just ls <- getListEntry e
    getName ls
    

--(a1 -> a1 -> c) -> (a2 -> f a1) -> a2 -> a2 -> f c
onA :: Applicative f => (a -> a -> c) -> (b -> f a) -> b -> b -> f c
onA = on . liftA2

printIO :: (MonadIO m, Show s) => s -> m () 
printIO = liftIO . print

{-
scanAccelerometer :: Double -> RawFilePath -> IO ()
scanAccelerometer scale path = do
  device <- newFromSysPath udev path
  runReaderT (scanAccelerometer scale) dev
-}

run :: MonadIO m => FilePath -> [String] -> m ()
run cmd args = runProcess_ $ proc ("/usr/bin" </> cmd) args

xrandr, xinput :: MonadIO m => [String] -> m ()
xrandr = run "xrandr"
xinput = run "xinput"

display, touchscreen :: String
display = "eDP1"
touchscreen = "SYNA7813:00 06CB:1785"

readOrientation :: Double -> DeviceIO GravityVector
readOrientation scale = do
    let scaled = scaledBy scale
    x <- scaled <$> getDimension "x"
    y <- scaled <$> getDimension "y"
    z <- scaled <$> getDimension "z"
    --`[x, y, z] <- traverse (scaled . getDimension) ["x", "y", "z"]
    pure $ GravityVector x y z
{-
readOrientation :: Double ->  DeviceIO ()
readOrientation scale  = do
    let scaled = scaledBy scale
    z <- scaled <$> getDimension "z"
    printIO z
    unless (gravity_g |+| z < closeEnoughToG) $ do
      --angle <- onA atan2 getDimension "y" "x"
      angle <- atan2 <$> getDimension "y" <*> getDimension "x"
      printIO $ 180.0 / pi * angle
      printIO $ orientationToArg $ orientationFor angle
      xrandr ["-o", orientationToArg $ orientationFor angle]
      xinput ["map-to-output", touchscreen, display]
-}

--readOrientation :: Double -> (GravityVector -> IO ()) -> DeviceIO ()
--(liftIO . putStrLn) "PROCEEDING"
    --[x, y] <- traverse getDimension ["x", "y"]
 {-
    liftIO $ putStrLn $ "X: " ++ show x
    liftIO $ putStrLn $ "Y: " ++ show y
    liftIO $ putStrLn $ "Z: " ++ show z
    --let angle = atan2 y x
    liftIO $ print $ 180 / pi * angle
    liftIO $ print $ orientationFor angle
-}

getDeviceFromPath :: RawFilePath -> UdevIO Device
getDeviceFromPath path = ask >>= lift . flip newFromSysPath path

runOnDevice :: RawFilePath -> DeviceIO a -> UdevIO a
runOnDevice path f = do
    dev <- getDeviceFromPath path
    liftIO $ runReaderT f dev

{-
runOnDevice ::  RawFilePath -> DeviceIO a -> UdevIO a
runOnDevice path f = do
    dev <- getDeviceFromPath path
    lift $ runReaderT f dev
-}

{-
runOnDevice :: UDev -> RawFilePath -> DeviceIO a -> IO a
runOnDevice udev devicePath f = do
    dev <- newFromSysPath udev devicePath
    runReaderT f dev
-}

runWithUDev :: UdevIO a -> IO a
runWithUDev f = withUDev $ runReaderT f

udevMain :: SysValue -> UdevIO Double
udevMain iio_device_name = do
    path <- getDevPath iio_device_name
    scale <- runOnDevice path $ parseDouble <$> getAccelAttr "scale"
    forever $ do
      GravityVector x y z <- runOnDevice path $ readOrientation scale
      threadDelay 1000000
      pure ()
    pure scale


main :: IO Double
main = runWithUDev $ udevMain "foo"
{-
do
  withUDev $ \ udev -> do
    path <- runWithUDev $ getDevPath iio_device_name
    --dev <- newFromSysPath udev path
    --scale <- runOnDevice udev path $ parseDouble <$> getAccelAttr "scale"
    scale <- runWithUDev $ runOnDevice path $ parseDouble <$> getAccelAttr "scale"
    forever $ do
      dev <- newFromSysPath udev path -- have to do this in the loop; values are static once device is created, despite getSysattrValue operating in IO
      -- I blame C.
      GravityVector x y z <- runWithUDev $ runOnDevice path $ readOrientation scale
      --runReaderT (readOrientation scale) dev
      threadDelay 1000000
-}
      
    --[x, y, z] <- traverse (fmap (twos_complement . parseNum) . getDimension dev) ["x", "y", "z"]
    --if (g + z < closeEnoughToG) then putStrLn "flat" else putStrLn "proceeding"
    --print x
    --print $ bs2i v
    --let Right (tc,"") = decimal @Word16 $ E.decodeUtf8 x
    --print (testBit x 15)
    --print $ (complement tc)
    --print $ x
    --let xval :: Int = if (testBit x 15) then negate $ fromIntegral $ negate x else fromIntegral x
    --putStrLn "not the momma"
    --print xval
