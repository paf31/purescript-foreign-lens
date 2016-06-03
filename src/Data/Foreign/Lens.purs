-- | A `lens`-compatible layer for `purescript-foreign`.

module Data.Foreign.Lens
  ( json
  , string
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
import Data.Foreign (Foreign, readArray, readInt, readNumber, readBoolean, readChar, readString, parseJSON)
import Data.Foreign.Index (index, prop) as F
import Data.Foreign.Keys (keys) as F
import Data.Lens (FoldP, traversed, to)
import Data.Monoid (class Monoid)

-- | A `Fold` which parses JSON.
json :: forall r. Monoid r => FoldP r String Foreign
json = to parseJSON <<< traversed

-- | A `Fold` which reads a `String`.
string :: forall r. Monoid r => FoldP r Foreign String
string = to readString <<< traversed

-- | A `Fold` which reads a `Char`.
char :: forall r. Monoid r => FoldP r Foreign Char
char = to readChar <<< traversed

-- | A `Fold` which reads a `Boolean`.
boolean :: forall r. Monoid r => FoldP r Foreign Boolean
boolean = to readBoolean <<< traversed

-- | A `Fold` which reads a `Number`.
number :: forall r. Monoid r => FoldP r Foreign Number
number = to readNumber <<< traversed

-- | A `Fold` which reads an `Int`.
int :: forall r. Monoid r => FoldP r Foreign Int
int = to readInt <<< traversed

-- | A `Fold` which reads an `Array`.
array :: forall r. Monoid r => FoldP r Foreign (Array Foreign)
array = to readArray <<< traversed

-- | A `Fold` which reads an object property.
prop :: forall r. Monoid r => String -> FoldP r Foreign Foreign
prop p = to (F.prop p) <<< traversed

-- | A `Fold` which reads an array index.
index :: forall r. Monoid r => Int -> FoldP r Foreign Foreign
index i = to (F.index i) <<< traversed

-- | A `Fold` which reads object keys.
keys :: forall r. Monoid r => FoldP r Foreign (Array String)
keys = to F.keys <<< traversed
