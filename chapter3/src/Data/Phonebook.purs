module Data.Phonebook where

import Control.Plus (empty)
import Data.List
import Data.Maybe

type Entry = { firstName :: String, lastName :: String, phone :: String }
type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons

findByNames :: String -> String -> PhoneBook -> Maybe Entry
findByNames firstName lastName = head <<< filter filterEntry where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName &&
                      entry.lastName == lastName

findByPhone :: String -> PhoneBook -> Maybe Entry
findByPhone phone = head <<< filter filterEntry where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.phone == phone

isInBook :: String -> String -> PhoneBook -> Boolean
isInBook firstName lastName = not <<< null <<< filter filterEntry where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName &&
                      entry.lastName == lastName

removeDuplicates :: PhoneBook -> PhoneBook
removeDuplicates = nubBy areEntriesEqual where
  areEntriesEqual :: Entry -> Entry -> Boolean
  areEntriesEqual e1 e2 = e1.firstName == e2.firstName &&
                          e1.lastName  == e2.lastName &&
                          e1.phone     == e2.phone
