{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables #-}
module Main (main) where

import Data.ByteString (ByteString) 
import qualified Data.ByteString as BS

import Data.Text (Text())
import Data.Text.Encoding as E
import Data.Text.Read (decimal, double, Reader())

import System.UDev.Context
import System.UDev.Device
import System.UDev.Enumerate
import System.UDev.List

import Data.Word (Word16())
import Data.Bits (testBit)

import Control.Concurrent(threadDelay)
import Control.Monad (unless, forever)

import Orientation (orientationFor)

twos_complement :: Word16 -> Int
twos_complement bits = if isNeg
  then -- negate the bits, turn it into an Int, then make it a negative Int
    negate $ fromIntegral $ negate bits 
  else
    fromIntegral bits
  where
    isNeg = testBit bits 15

getDimension :: Device -> ByteString -> IO Int
getDimension dev name = parseInt <$> getAccelAttr dev (BS.append name "_raw")
--getDimension dev v = getSysattrValue dev $ BS.concat ["in_accel_", v, "_raw"]

getAccelAttr :: Device -> ByteString -> IO ByteString
getAccelAttr dev name = getSysattrValue dev $ BS.append "in_accel_" name

parseInt :: ByteString -> Int
parseInt = twos_complement . fromReader . decimal . E.decodeUtf8

parseDouble :: ByteString -> Double
parseDouble = fromReader . double . E.decodeUtf8

--parseInt bs = n
--  where Right (n, "") = decimal $ E.decodeUtf8 bs

fromReader :: Either String (a, Text) -> a
fromReader (Right (n, "")) = n

main :: IO ()
main = forever $ foo *> threadDelay 1000000

g :: Double
g = 9.8 -- m/s^2

-- Just pick a number that you want to call close enough to laying flat that we should stop rotating the screen.
closeEnoughToG :: Double
closeEnoughToG = 0.3  -- m/s^2

foo :: IO ()
foo = do
  withUDev $ \ udev -> do
    e <- newEnumerate udev
    addMatchSubsystem e "iio"
    addMatchSysattr e "name" (Just "accel_3d")
    scanDevices e
    Just ls <- getListEntry e
    path  <- getName ls
    dev <- newFromSysPath udev path
    scale <- parseDouble <$> getAccelAttr dev "scale"
    let scaled i = scale * fromIntegral i
    --[x, y, z] <- traverse (fmap (twos_complement . parseNum) . getDimension dev) ["x", "y", "z"]
    z <- scaled <$> getDimension dev "z"
    --if (g + z < closeEnoughToG) then putStrLn "flat" else putStrLn "proceeding"
    unless (g + z < closeEnoughToG) $ putStrLn "PROCEEDING"
    [x, y] <- traverse (getDimension dev) ["x", "y"]
    --print x
    --print $ bs2i v
    --let Right (tc,"") = decimal @Word16 $ E.decodeUtf8 x
    --print (testBit x 15)
    --print $ (complement tc)
    --print $ x
    --let xval :: Int = if (testBit x 15) then negate $ fromIntegral $ negate x else fromIntegral x
    --putStrLn "not the momma"
    --print xval
    putStrLn $ "X: " ++ show x
    putStrLn $ "Y: " ++ show y
    putStrLn $ "Z: " ++ show z
    let angle = atan2 (fromIntegral y) (fromIntegral x)
    print $ 180 / pi * angle
    print $ orientationFor angle
