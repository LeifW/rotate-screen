{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Orientation (orientationFor) where

import qualified Data.IntervalMap.Generic.Strict as IM

instance Ord e => IM.Interval (e,e) e where
    lowerBound (a,_) = a
    upperBound (_,b) = b
    rightClosed _ = False

data Orientation = Portrait | Landscape | FlippedPortrait | FlippedLandscape deriving (Show, Eq)

type OrientationMap = IM.IntervalMap (Double,Double) Orientation

(|=>) :: v -> v -> (v, v)
b |=> e = (b, e)

ranges :: OrientationMap
ranges = IM.fromList $
  ((      -pi, -3/4 * pi), FlippedPortrait) :
  ((-3/4 * pi, -1/4 * pi), Landscape) :
  ((-1/4 * pi,  1/4 * pi), Portrait) :
  (( 1/4 * pi,  3/4 * pi), FlippedLandscape) :
  (( 3/4 * pi,        pi + epsilon), FlippedLandscape) :
  []
  where epsilon = 0.01 -- because the upper bound is non-inclusive; want to include the case where the angle = 180 degrees.

orientationFor :: Double -> Orientation
orientationFor angle = case IM.toList $ IM.containing ranges angle of
  [(_, o)] -> o 
  otherwise -> error $ "No unique range found for angle " ++ show angle ++ ": " ++ show otherwise
