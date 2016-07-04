module Exercises where

import Prelude ((==), (&&), ($), not)

import Data.Functor ((<$>))
import Data.List (head, filter)
import Data.Maybe (Maybe(..), isNothing)

import Data.AddressBook

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName book = not $ isNothing $ findEntry firstName lastName book
