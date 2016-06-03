module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (traverse_)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Lens (string, prop, array)
import Data.Lens (FoldP(), (^..), traversed)
import Data.Monoid (class Monoid)

doc :: Foreign
doc = toForeign { paras: [ { word: "Hello" }, { word: "World" } ] }

-- | This `FoldP` extracts all words appearing in a structure like the one above.
words :: forall r. Monoid r => FoldP r Foreign String
words = prop "paras"
    <<< array
    <<< traversed
    <<< prop "word"
    <<< string

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = traverse_ log (doc ^.. words)
