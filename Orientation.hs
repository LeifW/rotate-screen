{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Orientation (orientationFor, orientationToArg, Orientation) where

import qualified Data.IntervalMap.Generic.Strict as IM

instance Ord e => IM.Interval (e,e) e where
    lowerBound (a,_) = a
    upperBound (_,b) = b
    rightClosed _ = False

data Orientation = Portrait | Landscape | FlippedPortrait | FlippedLandscape deriving (Show, Eq)

-- To xrandr arg
orientationToArg :: Orientation -> String
orientationToArg Landscape        = "normal"
orientationToArg Portrait         = "left"
orientationToArg FlippedPortrait  = "right"
orientationToArg FlippedLandscape = "inverted"


type OrientationMap = IM.IntervalMap (Double, Double) Orientation

-- 1/4 pi radians == 45 degrees
ranges :: OrientationMap
ranges = IM.fromList $
  ((      -pi, -3/4 * pi), FlippedPortrait) :
  ((-3/4 * pi, -1/4 * pi), Landscape) :
  ((-1/4 * pi,  1/4 * pi), Portrait) :
  (( 1/4 * pi,  3/4 * pi), FlippedLandscape) :
  (( 3/4 * pi,        pi + epsilon), FlippedLandscape) :
  []
  where epsilon = 0.01 -- because the upper bound is non-inclusive; want to include the case where the angle = 180 degrees.

-- angle of rotation from having left edge of screen on bottom, in radians -> [portrait, landscape, inverted portrait, inverted landscape]
-- (the angle the gravity vector is pointing, in degrees from the x-axis)
--orientationFor :: Double -> Orientation
orientationFor :: Double -- ^ The angle the gravity vector is pointing, in degrees from the x-axis.
               -> Orientation
orientationFor θ = case IM.toList $ IM.containing ranges θ of
  [(_, o)] -> o 
  other-> error $ "No unique range found for angle " ++ show θ ++ ": " ++ show other
