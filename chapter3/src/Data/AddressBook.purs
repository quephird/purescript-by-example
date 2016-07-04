module Data.AddressBook where

import Prelude ((<>), (==), (&&), ($), (<<<),
                not)

import Control.Plus (empty)
import Data.Functor ((<$>))
import Data.List (List(..),
                  (:),
                  filter, head, nubBy, null)
import Data.Maybe (Maybe(..))

type Address =
  { street :: String
  , city :: String
  , state :: String }

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName &&
                      entry.lastName == lastName

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book
