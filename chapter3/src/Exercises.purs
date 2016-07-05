module Exercises where

import Prelude (class Eq,
                (==), (&&), ($), not)

import Data.List (nubBy)
import Data.Functor ((<$>))
import Data.List (head, filter)
import Data.Maybe (Maybe(..), isNothing)

import Data.AddressBook

-- 1.
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

-- 2.
entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName book = not $ isNothing $ findEntry firstName lastName book

-- 3.
sameName :: Entry -> Entry -> Boolean
sameName e1 e2 = e1.firstName == e2.firstName &&
                 e1.lastName == e2.lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameName
