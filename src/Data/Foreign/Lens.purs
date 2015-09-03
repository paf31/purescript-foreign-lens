-- | A `lens`-compatible layer for `purescript-foreign`.

module Data.Foreign.Lens 
  ( PartialGetter()
  , getter
  , get
  , getMap
  
  , json
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
    
import Data.Maybe
import Data.Maybe.First
import Data.Const
import Data.Either
import Data.Monoid
import Data.Functor.Contravariant

import qualified Data.Foreign as F
import qualified Data.Foreign.Index as F
import qualified Data.Foreign.Keys as F
   
data Void = Void

coerce :: forall f a b. (Contravariant f, Functor f) => f a -> f b
coerce = map absurd <<< cmap absurd
  where
  absurd :: forall a. Void -> a
  absurd a = absurd a
    
-- | An optic for getting zero or more values with possible failure.
type PartialGetter a s = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
    
-- | Run a `PartialGetter`, getting the first value.
get :: forall a s. PartialGetter a s -> s -> Maybe a
get g = runFirst <<< getMap (First <<< Just) g

-- | Run a `PartialGetter`, getting all values.
getMap :: forall a s m. (Monoid m) => (a -> m) -> PartialGetter a s -> s -> m
getMap f g = getConst <<< g (Const <<< f)
    
-- | Create a `PartialGetter` from a function which uses `Either` to indicate failure.
getter :: forall e a s. (s -> Either e a) -> PartialGetter a s
getter f g s = 
  case f s of
    Left _ -> coerce (pure unit)
    Right a -> coerce (g a)
   
-- | A `PartialGetter` which parses JSON.
json :: PartialGetter F.Foreign String
json = getter F.parseJSON
   
-- | A `PartialGetter` which reads a `String`.
string :: PartialGetter String F.Foreign
string = getter F.readString
   
-- | A `PartialGetter` which reads a `Char`.
char :: PartialGetter Char F.Foreign
char = getter F.readChar
   
-- | A `PartialGetter` which reads a `Boolean`.
boolean :: PartialGetter Boolean F.Foreign
boolean = getter F.readBoolean
   
-- | A `PartialGetter` which reads a `Number`.
number :: PartialGetter Number F.Foreign
number = getter F.readNumber
   
-- | A `PartialGetter` which reads an `Int`.
int :: PartialGetter Int F.Foreign
int = getter F.readInt
   
-- | A `PartialGetter` which reads an `Array`.
array :: PartialGetter (Array F.Foreign) F.Foreign
array = getter F.readArray
   
-- | A `PartialGetter` which reads an object property.
prop :: String -> PartialGetter F.Foreign F.Foreign
prop p = getter (F.prop p)
   
-- | A `PartialGetter` which reads an array index.
index :: Int -> PartialGetter F.Foreign F.Foreign
index i = getter (F.index i)
   
-- | A `PartialGetter` which reads object keys.
keys :: PartialGetter (Array String) F.Foreign
keys = getter F.keys