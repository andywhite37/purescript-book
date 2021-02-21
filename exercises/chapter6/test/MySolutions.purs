module Test.MySolutions where

import Prelude
import Data.Array (length, nub, nubByEq, nubEq)
import Data.Either.Nested (in1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable, hash, hashEqual)
import Data.Maybe (fromJust)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, wrap)
import Math as Math

newtype Point
  = Point { x :: Number, y :: Number }

instance showPoint :: Show Point where
  show (Point a) = "(" <> show a.x <> ", " <> show a.y <> ")"

newtype Complex
  = Complex { real :: Number, imaginary :: Number }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> sign <> show (Math.abs imaginary) <> "i"
    where
    sign :: String
    sign = if imaginary >= 0.0 then "+" else "-"

derive instance eqComplex :: Eq Complex

-- without Newtype
--instance semiringComplex :: Semiring Complex where
--zero = Complex { real: 0.0, imaginary: 0.0 }
--one = Complex { real: 1.0, imaginary: 1.0 }
--add (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = Complex { real: r1 + r2, imaginary: i1 + i2 }
--mul (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = Complex { real: r1 * r2 - i1 * i2, imaginary: r1 * i2 + r2 * i1 }
derive instance newtypeComplex :: Newtype Complex _

-- with Newtype
instance semiringComplex :: Semiring Complex where
  zero = wrap { real: 0.0, imaginary: 0.0 }
  one = wrap { real: 1.0, imaginary: 0.0 }
  add = over2 Complex (\c1 c2 -> { real: c1.real + c2.real, imaginary: c1.imaginary + c2.imaginary })
  mul = over2 Complex (\c1 c2 -> { real: c1.real * c2.real - c1.imaginary * c2.imaginary, imaginary: c1.real * c2.imaginary + c2.real * c1.imaginary })

--instance ringComplex :: Ring Complex where
--sub = over2 Complex (\c1 c2 -> { real: c1.real - c2.real, imaginary: c1.imaginary - c2.imaginary })
derive newtype instance ringComplex :: Ring Complex

derive instance eqPoint :: Eq Point

derive instance ordPoint :: Ord Point

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

derive instance eqShape :: Eq Shape

derive instance ordShape :: Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

data NonEmpty a
  = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty a as) = "(NonEmpty " <> show a <> show as <> ")"

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty l ls) (NonEmpty r rs) = l == r && ls == rs

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty l ls) (NonEmpty r rs) = NonEmpty l (ls <> [ r ] <> rs)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a as) = NonEmpty (f a) (f <$> as)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f init (NonEmpty a as) = f a (foldr f init as)
  foldl f init (NonEmpty a as) = foldl f (f init a) as
  foldMap f (NonEmpty a as) = f a <> foldMap f as

data Extended a
  = Infinite
  | Finite a

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite (Finite _) = false
  eq (Finite _) Infinite = false
  eq (Finite a) (Finite b) = a == b

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite a) (Finite b) = compare a b

data OneMore f a
  = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr f init (OneMore a fa) = f a (foldr f init fa)
  foldl f init (OneMore a fa) = foldl f (f init a) fa
  foldMap f (OneMore a fa) = f a <> foldMap f fa

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum

class
  Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply
  = Multiply Int

derive newtype instance showMultiply :: Show Multiply

derive newtype instance eqMultiply :: Eq Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply a) (Multiply b) = Multiply (a * b)

instance monoidMultiply :: Monoid Multiply where
  mempty = (Multiply 1)

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply a) b = a * b

instance actionMultiplyString :: Action Multiply String where
  act (Multiply a) str = power str a

instance actionMultiplyArray :: Action Multiply a => Action Multiply (Array a) where
  act a as = act a <$> as

newtype Self m
  = Self m

derive newtype instance eqSelf :: Eq m => Eq (Self m)

derive newtype instance showSelf :: Show m => Show (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

arrayHasDuplicates :: ∀ a. Hashable a => Eq a => Array a -> Boolean
arrayHasDuplicates xs =
  let
    equalsByHashAndEq :: a -> a -> Boolean
    equalsByHashAndEq a b = hashEqual a b && a == b

    deduped :: Array a
    deduped = nubByEq equalsByHashAndEq xs
  in
    length deduped /= length xs

newtype Hour
  = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour a) (Hour b) = a `mod` 12 == b `mod` 12

instance hashableHour :: Hashable Hour where
  hash (Hour h) = hash (h `mod` 12)
