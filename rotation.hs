{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, QuasiQuotes  #-}
module Main (main) where

import Data.ByteString (ByteString) 
import qualified Data.ByteString as BS

import Data.Text (Text())
import Data.Text.Encoding as E
import Data.Text.Read (decimal, double)

import System.UDev.Context
import System.UDev.Device
import System.UDev.Enumerate
import System.UDev.List

import Data.Word (Word16())
import Data.Bits (testBit)

import Control.Concurrent(threadDelay)
import Control.Monad (unless, forever)

import Control.Monad.Reader

import Orientation (orientationFor)
--import Units

import Data.Metrology
import Data.Metrology.SI

import Data.Constants.Mechanics

import Data.Metrology.Show ()

type DeviceIO = ReaderT Device IO

twos_complement :: Word16 -> Int
twos_complement bits = if isNeg
  then -- negate the bits, turn it into an Int, then make it a negative Int
    negate $ fromIntegral $ negate bits 
  else
    fromIntegral bits
  where
    isNeg = testBit bits 15

accelAttr :: ByteString -> Device -> IO ByteString
accelAttr name dev = getSysattrValue dev (BS.append "in_accel_" name)

getAccelAttr :: ByteString -> DeviceIO ByteString
getAccelAttr name = ask >>= lift . accelAttr name

getDimension :: ByteString -> DeviceIO Int
getDimension name = parseInt <$> getAccelAttr (BS.append name "_raw")
--getDimension dev v = getSysattrValue dev $ BS.concat ["in_accel_", v, "_raw"]


parseInt :: ByteString -> Int
parseInt = twos_complement . fromReader . decimal . E.decodeUtf8

parseDouble :: ByteString -> Double
parseDouble = fromReader . double . E.decodeUtf8

--parseInt bs = n
--  where Right (n, "") = decimal $ E.decodeUtf8 bs

fromReader :: Either String (a, Text) -> a
fromReader (Right (n, "")) = n

--main :: IO ()
--main = forever $ foo *> threadDelay 1000000

--g :: Double
--g = 9.8 -- m/s^2

scaledBy :: Double -> Int -> Acceleration
scaledBy scale i = fromIntegral i *| (scale % [si| m/s^2 |])

-- Just pick a number that you want to call close enough to laying flat that we should stop rotating the screen.
closeEnoughToG :: Acceleration
closeEnoughToG = 0.3 % [si| m/s^2 |]

getDev :: UDev -> IO Device
getDev udev = do
    e <- newEnumerate udev
    addMatchSubsystem e "iio"
    addMatchSysattr e "name" (Just "accel_3d")
    scanDevices e
    Just ls <- getListEntry e
    path <- getName ls
    newFromSysPath udev path

scanAccelerometer :: Double -> DeviceIO ()
scanAccelerometer scale = do
    let scaled = scaledBy scale
    z <- scaled <$> getDimension "z"
    unless (gravity_g |+| z < closeEnoughToG) $ (liftIO . putStrLn) "PROCEEDING"
    [x, y] <- traverse getDimension ["x", "y"]
    liftIO $ putStrLn $ "X: " ++ show x
    liftIO $ putStrLn $ "Y: " ++ show y
    liftIO $ putStrLn $ "Z: " ++ show z
    let angle = atan2 (fromIntegral y) (fromIntegral x)
    liftIO $ print $ 180 / pi * angle
    liftIO $ print $ orientationFor angle

main :: IO ()
main = do
  withUDev $ \ udev -> do
    dev <- getDev udev
    scale <- parseDouble <$> accelAttr "scale" dev
    forever $ do
      runReaderT (scanAccelerometer scale) dev
      threadDelay 1000000
      
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
