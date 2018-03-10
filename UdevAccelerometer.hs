{-# LANGUAGE OverloadedStrings #-}
module UdevAccelerometer (SysValue(), GravityVector(GravityVector), UdevIO, DeviceIO, parseDouble, runOnDevice, runWithUDevDevice, getAccelAttr, readOrientation )  where

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

import Control.Monad.Reader

data UDevDevice = UDevDevice {
  _udev :: UDev,
  _devicePath :: RawFilePath
}

type DeviceIO = ReaderT Device IO
type UdevIO = ReaderT UDevDevice IO

twos_complement :: Word16 -> Int
twos_complement bits = if isNeg
  then -- negate the bits, turn it into an Int, then make it a negative Int
    negate $ fromIntegral $ negate bits 
  else
    fromIntegral bits
  where
    isNeg = testBit bits 15

data GravityVector = GravityVector {
  _x :: Double,
  _y :: Double,
  _z :: Double
} deriving Show

accelAttr :: ByteString -> Device -> IO ByteString
accelAttr name dev = getSysattrValue dev (BS.append "in_accel_" name)

getAccelAttr :: ByteString -> DeviceIO  ByteString
getAccelAttr name = ask >>= lift . accelAttr name

-- Everything that consumes these wants a Double, so I'll go ahead and convert the Int to a Double in here.
getDimension :: ByteString -> DeviceIO Double
getDimension name = (fromIntegral . parseInt) <$> getAccelAttr (BS.append name "_raw")


parseInt :: ByteString -> Int
parseInt = twos_complement . fromReader . decimal . E.decodeUtf8

parseDouble :: ByteString -> Double
parseDouble = fromReader . double . E.decodeUtf8

--partial
fromReader :: Either String (a, Text) -> a
fromReader (Right (n, "")) = n

getDevPath :: UDev -> SysValue -> IO RawFilePath
getDevPath udev devName = do
    e <- newEnumerate udev
    addMatchSubsystem e "iio"
    addMatchSysattr e "name" (Just devName)
    scanDevices e
    ls <- getListEntry e --partial
    getName ls
    
readOrientation :: DeviceIO GravityVector
readOrientation = do
    [x, y, z] <- traverse getDimension ["x", "y", "z"]
    pure $ GravityVector x y z

getDevice :: UdevIO Device
getDevice = do
  UDevDevice udev path <- ask
  lift $ newFromSysPath udev path

runOnDevice :: DeviceIO a -> UdevIO a
runOnDevice f = do
    dev <- getDevice
    liftIO $ runReaderT f dev

runWithUDevDevice :: SysValue -> UdevIO a -> IO a
runWithUDevDevice devName f = withUDev $ \udev -> do
  path <- getDevPath udev devName
  runReaderT f $ UDevDevice udev path
