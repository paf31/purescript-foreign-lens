-- | A `lens`-compatible layer for `purescript-foreign`.

module Data.Foreign.Lens
  ( string
  , char
  , boolean
  , number
  , int
  , array
  , prop
  , index
  , keys
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Foreign (Foreign, readArray, readInt, readNumber, readBoolean, readChar, readString)
import Data.Foreign.Index (index, readProp) as F
import Data.Foreign.Keys (keys) as F
import Data.Lens (Fold', traversed, to)
import Data.Monoid (class Monoid)

-- | A `Fold` which reads a `String`.
string :: forall r. Monoid r => Fold' r Foreign String
string = to (runExcept <<< readString) <<< traversed

-- | A `Fold` which reads a `Char`.
char :: forall r. Monoid r => Fold' r Foreign Char
char = to (runExcept <<< readChar) <<< traversed

-- | A `Fold` which reads a `Boolean`.
boolean :: forall r. Monoid r => Fold' r Foreign Boolean
boolean = to (runExcept <<< readBoolean) <<< traversed

-- | A `Fold` which reads a `Number`.
number :: forall r. Monoid r => Fold' r Foreign Number
number = to (runExcept <<< readNumber) <<< traversed

-- | A `Fold` which reads an `Int`.
int :: forall r. Monoid r => Fold' r Foreign Int
int = to (runExcept <<< readInt) <<< traversed

-- | A `Fold` which reads an `Array`.
array :: forall r. Monoid r => Fold' r Foreign (Array Foreign)
array = to (runExcept <<< readArray) <<< traversed

-- | A `Fold` which reads an object property.
prop :: forall r. Monoid r => String -> Fold' r Foreign Foreign
prop p = to (runExcept <<< F.readProp p) <<< traversed

-- | A `Fold` which reads an array index.
index :: forall r. Monoid r => Int -> Fold' r Foreign Foreign
index i = to (runExcept <<< (_ `F.index` i)) <<< traversed

-- | A `Fold` which reads object keys.
keys :: forall r. Monoid r => Fold' r Foreign (Array String)
keys = to (runExcept <<< F.keys) <<< traversed
