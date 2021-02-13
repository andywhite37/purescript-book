module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.List (head, filter, nubBy, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry = _.address.street >>> (_ == street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter (\b -> b.firstName == firstName && b.lastName == lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy equalsEntry
  where
  equalsEntry :: Entry -> Entry -> Boolean
  equalsEntry e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
