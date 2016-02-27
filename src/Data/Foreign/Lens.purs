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
    
import Data.Lens
import Data.Maybe
import Data.Maybe.First
import Data.Const
import Data.Either
import Data.Monoid
import Data.Traversable (Traversable)

import qualified Data.Foreign as F
import qualified Data.Foreign.Index as F
import qualified Data.Foreign.Keys as F
   
-- | A `forall r. FoldP r` which parses JSON.
json :: forall r. (Monoid r) => FoldP r String F.Foreign
json = to F.parseJSON <<< traversed
   
-- | A `forall r. FoldP r` which reads a `String`.
string :: forall r. (Monoid r) => FoldP r F.Foreign String
string = to F.readString <<< traversed
   
-- | A `forall r. FoldP r` which reads a `Char`.
char :: forall r. (Monoid r) => FoldP r F.Foreign Char
char = to F.readChar <<< traversed
   
-- | A `forall r. FoldP r` which reads a `Boolean`.
boolean :: forall r. (Monoid r) => FoldP r F.Foreign Boolean
boolean = to F.readBoolean <<< traversed
   
-- | A `forall r. FoldP r` which reads a `Number`.
number :: forall r. (Monoid r) => FoldP r F.Foreign Number
number = to F.readNumber <<< traversed
   
-- | A `forall r. FoldP r` which reads an `Int`.
int :: forall r. (Monoid r) => FoldP r F.Foreign Int
int = to F.readInt <<< traversed
   
-- | A `forall r. FoldP r` which reads an `Array`.
array :: forall r. (Monoid r) => FoldP r F.Foreign (Array F.Foreign)
array = to F.readArray <<< traversed
   
-- | A `forall r. FoldP r` which reads an object property.
prop :: forall r. (Monoid r) => String -> FoldP r F.Foreign F.Foreign
prop p = to (F.prop p) <<< traversed
   
-- | A `forall r. FoldP r` which reads an array index.
index :: forall r. (Monoid r) => Int -> FoldP r F.Foreign F.Foreign
index i = to (F.index i) <<< traversed
   
-- | A `forall r. FoldP r` which reads object keys.
keys :: forall r. (Monoid r) => FoldP r F.Foreign (Array String)
keys = to F.keys <<< traversed
