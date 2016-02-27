module Test.Main where

import Prelude

import Data.Monoid (Monoid)
import Data.Lens (FoldP(), (^..), traversed)
import Data.Foreign
import Data.Foreign.Lens
import Data.Traversable (traverse)

import qualified Data.Array as A

import Control.Monad.Eff.Console

main = do
  let doc = toForeign { words: [ { word: "Hello" }, { word: "World" } ] }
    
  let bars :: forall r. (Monoid r) => FoldP r Foreign String
      bars = prop "words" 
         <<< array 
         <<< traversed 
         <<< prop "word" 
         <<< string
    
  traverse print $ doc ^.. bars
