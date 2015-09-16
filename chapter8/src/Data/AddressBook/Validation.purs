module Data.AddressBook.Validation where

import Prelude

import Data.Array
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable
import Data.Tuple

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

type ErrorMessage = String
type FieldValue = String

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType

data ValidationError = ValidationError Field ErrorMessage
type ValidationErrors = Array ValidationError

nonEmpty :: Field -> FieldValue -> V ValidationErrors Unit
nonEmpty field "" = invalid [ValidationError field "Field cannot be empty"]
nonEmpty _     _  = pure unit

nonEmptyPhoneNumber :: PhoneNumber -> V ValidationErrors Unit
nonEmptyPhoneNumber (PhoneNumber o) = nonEmpty (PhoneField o."type") o.number

arrayNonEmpty :: forall a. Field -> Array a -> V ValidationErrors Unit
arrayNonEmpty field [] = invalid [ValidationError field "Must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Int -> String -> V ValidationErrors Unit
lengthIs field len value | S.length value /= len = invalid [ValidationError field $ "Must have length " ++ show len]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: R.Regex
phoneNumberRegex =
  R.regex
    "^\\d{3}-\\d{3}-\\d{4}$"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

matches :: Field -> R.Regex -> String -> V ValidationErrors Unit
matches _     regex value | R.test regex value = pure unit
matches field _     _     = invalid [ValidationError field "Value does not match the required format"]

validateAddress :: Address -> V ValidationErrors Address
validateAddress (Address o) =
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V ValidationErrors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o."type") phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V ValidationErrors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.address
         <*> (traverse nonEmptyPhoneNumber o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either ValidationErrors Person
validatePerson' p = runV Left Right $ validatePerson p
