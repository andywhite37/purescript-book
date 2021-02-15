module Test.MySolutions where

import Prelude
import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Person (Person)
import Data.Picture (Point, Shape(..), showShape)
import Math as Math

factorial :: Int -> Int
factorial 0 = 1

factorial 1 = 1

factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1

binomial 0 _ = 0

binomial n k
  | n < k = 0
  | otherwise = (factorial n) / ((factorial k) * factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1

pascal 0 _ = 0

pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

fromSingleton :: ∀ a. a -> Array a -> a
fromSingleton _ [ a ] = a

fromSingleton a _ = a

circleAtOrigin :: Shape
circleAtOrigin = Circle { x: 0.0, y: 0.0 } 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle { x: 0.0, y: 0.0 } $ r * 2.0

doubleScaleAndCenter (Rectangle _ w h) = Rectangle { x: 0.0, y: 0.0 } (w * 2.0) (h * 2.0)

doubleScaleAndCenter (Line p1 p2) =
  let
    oldW = Math.abs (p1.x - p2.x)

    oldH = Math.abs (p1.y - p2.y)

    w = 2.0 * oldW

    h = 2.0 * oldH
  in
    Line { x: -oldW, y: -oldH } { x: -oldW + w, y: -oldH + h }

doubleScaleAndCenter (Text p text) = Text { x: 0.0, y: 0.0 } text

doubleScaleAndCenter (Clipped _ w h p) = Clipped { x: 0.0, y: 0.0 } (w * 2.0) (h * 2.0) p

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text

shapeText _ = Nothing

newtype Watt
  = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt v) = Watt (v * i)

type Picture
  = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

area :: Shape -> Number
area (Circle _ r) = Math.pi * r * r

area (Rectangle _ w h) = w * h

area (Line _ _) = 0.0

area (Text _ _) = 0.0

area (Clipped _ w h _) = w * h
