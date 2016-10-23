module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Lens (string, prop, array)
import Data.Lens (Fold', sequenceOf_, traversed, to)
import Data.Monoid (class Monoid)

doc :: Foreign
doc = toForeign { paras: [ { word: "Hello" }, { word: "World" } ] }

-- | This `Fold'` extracts all words appearing in a structure like the one above.
words :: forall r. Monoid r => Fold' r Foreign String
words = prop "paras"
    <<< array
    <<< traversed
    <<< prop "word"
    <<< string

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = sequenceOf_ (words <<< to log) doc
