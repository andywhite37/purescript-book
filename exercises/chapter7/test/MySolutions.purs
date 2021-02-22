module Test.MySolutions where

import Prelude
import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)

addMaybe :: ∀ a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: ∀ a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: ∀ a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divMaybe :: ∀ a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 (/)

addApply :: ∀ f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: ∀ f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: ∀ f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: ∀ f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/)

combineMaybe :: ∀ f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing

combineMaybe (Just fa) = Just <$> fa

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-z]{2}$" ignoreCase

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" ignoreCase

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- matches "Street" nonEmptyRegex a.street
  city <- matches "City" nonEmptyRegex a.city
  state <- matches "State" stateRegex a.state
  in address street city state

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

--instance eqTree :: Eq a => Eq (Tree a) where
--eq Leaf Leaf = true
--eq Leaf (Branch _ _ _) = false
--eq (Branch _ _ _) Leaf = false
--eq (Branch al a ar) (Branch bl b br) = al == bl && a == b && ar == br
derive instance eqTree :: Eq a => Eq (Tree a)

--instance showTree :: Show a => Show (Tree a) where
--show Leaf = "Leaf"
--show (Branch l a r) = "(Branch " <> show l <> " " <> show a <> " " <> show r <> ")"
derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show a = genericShow a

derive instance functorTree :: Functor Tree

instance foldableTree :: Foldable Tree where
  foldr _ init Leaf = init
  foldr f init (Branch l a r) =
    let
      acc1 = foldr f init r

      acc2 = f a acc1

      acc3 = foldr f acc2 l
    in
      acc3
  foldl _ init Leaf = init
  foldl f init (Branch l a r) =
    let
      acc1 = foldl f init l

      acc2 = f acc1 a

      acc3 = foldl f acc2 r
    in
      acc3
  foldMap _ Leaf = mempty
  foldMap f (Branch l a r) = foldMap f l <> f a <> foldMap f r

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l a r) = Branch <$> traverse f l <*> f a <*> traverse f r
  sequence = traverse identity

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf

traversePreOrder f (Branch l a r) = ado
  a' <- f a
  l' <- traversePreOrder f l
  r' <- traversePreOrder f r
  in Branch l' a' r'

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf

traversePostOrder f (Branch l a r) = ado
  l' <- traversePostOrder f l
  r' <- traversePostOrder f r
  a' <- f a
  in Branch l' a' r'

type Person
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

person :: String -> String -> Maybe Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress p =
  person <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: ∀ a m f. Traversable f => Applicative m => f (m a) -> m (f a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: ∀ a b m f. Traversable f => Applicative m => (a -> m b) -> f a -> m (f b)
traverseUsingSequence f fa = sequence $ f <$> fa
