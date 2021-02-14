module Test.MySolutions where

import Prelude
import Control.MonadPlus (guard)
import Data.Array (catMaybes, cons, filter, foldl, foldr, head, last, length, uncons, (..), (:))
import Data.Foldable (product)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path(..), filename, isDirectory, ls)
import Data.String (Pattern(..), split)

isEven :: Int -> Boolean
isEven 0 = true

isEven 1 = false

isEven (-1) = false

isEven n = if n > 1 then isEven $ n - 2 else isEven $ n + 2

countEven :: Array Int -> Int
countEven ints = countEven' 0 ints
  where
  countEven' :: Int -> Array Int -> Int
  countEven' acc ints' = case uncons ints' of
    Just { head, tail } -> countEven' (acc + if isEven head then 1 else 0) tail
    Nothing -> acc

squared :: Array Number -> Array Number
squared ns = (\n -> n * n) <$> ns

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\n -> n >= 0.0)

infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite ns = (\n -> n >= 0.0) <$?> ns

factors :: Int -> Array (Array Int)
factors n =
  let
    pairs = do
      i <- 1 .. n
      j <- i .. n
      pure [ i, j ]
  in
    filter (\p -> product p == n) pairs

factorsGuard :: Int -> Array (Array Int)
factorsGuard n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

isPrime :: Int -> Boolean
isPrime n = n > 1 && (length $ factors n) == 1

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

factorize :: Int -> Array Int
factorize n = factorize' 2 n []
  where
  factorize' :: Int -> Int -> Array Int -> Array Int
  factorize' _ 1 result = result

  factorize' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
      if remainder == 0 then
        factorize' (divisor) (quot dividend divisor) (cons divisor result)
      else
        factorize' (divisor + 1) dividend result

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc b -> acc && b) true

fibTailRec :: Int -> Int
fibTailRec n = fib' 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int
  fib' count n1 n2 =
    if count == n then
      n1 + n2
    else
      fib' (count + 1) (n1 + n2) n1

reverse :: ∀ a. Array a -> Array a
reverse = foldl (\acc a -> acc <> [ a ]) []

allFiles :: Path -> Array Path
allFiles path =
  path
    : do
        childPath <- ls path
        allFiles childPath

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not <<< isDirectory) (allFiles path)

whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ whereIsAll $ allFiles path
  where
  whereIsAll :: Array Path -> Array Path
  whereIsAll paths = do
    path' <- paths
    childPath <- ls path'
    guard $ (fromMaybe "" $ last $ split (Pattern "/") $ filename childPath) == fileName
    pure path'

largestSmallest :: Path -> Array Path
largestSmallest path =
  let
    choose :: (Int -> Int -> Boolean) -> Maybe Path -> Path -> Maybe Path
    choose f (Just l@(File _ sl)) r@(File _ sr) = if f sl sr then Just l else Just r

    choose _ (Just l@(File _ _)) _ = Just l

    choose _ _ r@(File _ _) = Just r

    choose _ _ _ = Nothing

    smaller :: Path -> Path -> Boolean
    smaller _ _ = true

    result :: { l :: Maybe Path, s :: Maybe Path }
    result = foldr (\p { l, s } -> { l: choose (>) l p, s: choose (<) s p }) { l: Nothing, s: Nothing } $ onlyFiles path
  in
    -- could also use catMaybes with nub to remove duplicate
    case result of
      { l: Just l, s: Just s } -> if filename l == filename s then [ l ] else [ l, s ]
      { l: Just l, s: Nothing } -> [ l ]
      { l: Nothing, s: Just s } -> [ s ]
      { l: Nothing, s: Nothing } -> []
