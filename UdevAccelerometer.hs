{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
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

import Control.Monad.Reader

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

data GravityVector a = GravityVector {
  _x :: a,
  _y :: a,
  _z :: a
} deriving (Show, Functor)

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

--parseInt bs = n
--  where Right (n, "") = decimal $ E.decodeUtf8 bs

--partial
fromReader :: Either String (a, Text) -> a
fromReader (Right (n, "")) = n

getDevPath:: SysValue -> UdevIO RawFilePath
getDevPath devName = ask >>= lift . devPath devName

devPath :: SysValue -> UDev -> IO RawFilePath
devPath devName udev = do
    e <- newEnumerate udev
    addMatchSubsystem e "iio"
    addMatchSysattr e "name" (Just devName)
    scanDevices e
    Just ls <- getListEntry e --partial
    getName ls
    
readOrientation :: DeviceIO (GravityVector Double)
readOrientation = do
    --x <- getDimension "x"
    --y <- getDimension "y"
    --z <- getDimension "z"
    [x, y, z] <- traverse getDimension ["x", "y", "z"]
    pure $ GravityVector x y z

getDeviceFromPath :: RawFilePath -> UdevIO Device
getDeviceFromPath path = ask >>= lift . flip newFromSysPath path

runOnDevice :: RawFilePath -> DeviceIO a -> UdevIO a
runOnDevice path f = do
    dev <- getDeviceFromPath path
    liftIO $ runReaderT f dev

runWithUDev :: UdevIO a -> IO a
runWithUDev f = withUDev $ runReaderT f
