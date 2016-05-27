module Data.AddressBook where

import Prelude

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street ++ ", " ++ addr.city ++ ", " ++ addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons


-- 3.1 (Easy) Test your understanding of the findEntry function by writing down
-- the types of each of its major subexpressions. For example, the type of the
-- head function as used is specialized to List Entry -> Maybe Entry.
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


-- 3.2 (Medium) Write a function which looks up an Entry given a street
-- address, by reusing the existing code in findEntry. Test your function in PSCi.
findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress addr = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry e = e.address.street == addr.street && e.address.city == addr.city && e.address.state == addr.state


-- 3.3 (Medium) Write a function which tests whether a name appears in a
-- AddressBook, returning a Boolean value. Hint: Use PSCi to find the type of the
-- Data.List.null function, which test whether a list is empty or not.
hasEntry :: String -> String -> AddressBook -> Boolean
hasEntry firstName lastName = not <<< null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


-- 3.4 (Difficult) Write a function removeDuplicates which removes duplicate
-- address book entries with the same first and last names. Hint: Use PSCi to find
-- the type of the Data.List.nubBy function, which removes duplicate elements from
-- a list based on an equality predicate.
removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates firstName lastName = nubBy isEqual
  where
  isEqual :: Entry -> Entry -> Boolean
  isEqual a b = a.firstName == b.firstName && a.lastName == b.lastName
