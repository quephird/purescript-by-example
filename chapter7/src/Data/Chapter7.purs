module Chapter7 where

import Control.Apply ((*>), lift2)
import Data.Maybe
import qualified Data.String.Regex as R
import Data.Validation

import Data.AddressBook.Validation

-- First set

maybeAdd :: forall n. (Semiring n) => Maybe n -> Maybe n -> Maybe n
maybeAdd a b = lift2 (+) a b
maybeMultiply :: forall n. (Semiring n) => Maybe n -> Maybe n -> Maybe n
maybeMultiply a b = lift2 (*) a b
maybeSubtract :: forall n. (Ring n) => Maybe n -> Maybe n -> Maybe n
maybeSubtract a b = lift2 (-) a b
maybeDivide :: forall n. (ModuloSemiring n) => Maybe n -> Maybe n -> Maybe n
maybeDivide a b = lift2 (/) a b

-- Second set

stateRegex :: R.Regex
stateRegex =
  R.regex
    "^[a-zA-Z]{2}$"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

validateState :: String -> V Errors String
validateState s = matches "State code" stateRegex s *> pure s

nonBlankRegex :: R.Regex
nonBlankRegex =
  R.regex
    "\\S.*"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false
    }

validateNonBlankString :: String -> String -> V Errors String
validateNonBlankString f s = matches f nonBlankRegex s *> pure s
