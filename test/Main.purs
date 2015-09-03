module Test.Main where

import Prelude

import Data.Foreign
import Data.Foreign.Lens
import Data.Traversable (traverse)

import qualified Data.Array as A

import Control.Monad.Eff.Console

main = do
  let doc =
    "{                    \
    \  \"foo\": [         \
    \    {                \
    \      \"bar\": true  \
    \    },               \
    \    {                \
    \      \"bar\": false \
    \    }                \
    \  ]                  \
    \}"
    
  let bars :: PartialGetter Boolean String
      bars = json 
             <<< prop "foo" 
             <<< array 
             <<< traverse 
             <<< prop "bar" 
             <<< boolean
    
  print (getMap A.singleton bars doc)
