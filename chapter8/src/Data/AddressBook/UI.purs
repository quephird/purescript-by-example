module Data.AddressBook.UI where

import Prelude

import DOM

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

import Control.Bind

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Console

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

toErrorSelector :: Field -> String
toErrorSelector f =
  "#error" ++ go f where
  go f = case f of
    FirstNameField -> "FirstName"
    LastNameField  -> "LastName"
    StreetField    -> "Street"
    CityField      -> "City"
    StateField     -> "State"
    PhoneField HomePhone -> "HomePhone"
    PhoneField WorkPhone -> "WorkPhone"
    PhoneField CellPhone -> "CellPhone"


displayValidationErrors :: forall eff. ValidationErrors -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
displayValidationErrors errs = do
  foreachE errs $ \(ValidationError field err) -> do
    Just errorDiv <- querySelector $ toErrorSelector field
    divElement <- createElement "div"
    errText <- setText err divElement
    alertClass <- addClass "alert" errText
    alert <- addClass "alert-danger" alertClass

    alert `appendChild` errorDiv
    return unit

  return unit

validateControls :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) (Either ValidationErrors Person)
validateControls = do
  log "Running validators"

  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> (address <$> valueOf "#inputStreet"
                           <*> valueOf "#inputCity"
                           <*> valueOf "#inputState")
              <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                           , phoneNumber WorkPhone <$> valueOf "#inputWorkPhone"
                           , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                           ]

  return $ validatePerson' p

clearValidationErrors :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
clearValidationErrors = do
  Just errorDivs <- querySelectorAll "div[id^='error']"
  foreachE errorDivs $ \errorDiv -> do
    setInnerHTML "" errorDiv
    return unit

  return unit

validateAndUpdateUI :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  clearValidationErrors

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> displayValidationErrors errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  b <- body
  addEventListener "change" validateAndUpdateUI b

  return unit
