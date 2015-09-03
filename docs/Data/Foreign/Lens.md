## Module Data.Foreign.Lens

A `lens`-compatible layer for `purescript-foreign`.

#### `PartialGetter`

``` purescript
type PartialGetter a s = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
```

An optic for getting zero or more values with possible failure.

#### `get`

``` purescript
get :: forall a s. PartialGetter a s -> s -> Maybe a
```

Run a `PartialGetter`, getting the first value.

#### `getMap`

``` purescript
getMap :: forall a s m. (Monoid m) => (a -> m) -> PartialGetter a s -> s -> m
```

Run a `PartialGetter`, getting all values.

#### `getter`

``` purescript
getter :: forall e a s. (s -> Either e a) -> PartialGetter a s
```

Create a `PartialGetter` from a function which uses `Either` to indicate failure.

#### `json`

``` purescript
json :: PartialGetter Foreign String
```

A `PartialGetter` which parses JSON.

#### `string`

``` purescript
string :: PartialGetter String Foreign
```

A `PartialGetter` which reads a `String`.

#### `char`

``` purescript
char :: PartialGetter Char Foreign
```

A `PartialGetter` which reads a `Char`.

#### `boolean`

``` purescript
boolean :: PartialGetter Boolean Foreign
```

A `PartialGetter` which reads a `Boolean`.

#### `number`

``` purescript
number :: PartialGetter Number Foreign
```

A `PartialGetter` which reads a `Number`.

#### `int`

``` purescript
int :: PartialGetter Int Foreign
```

A `PartialGetter` which reads an `Int`.

#### `array`

``` purescript
array :: PartialGetter (Array Foreign) Foreign
```

A `PartialGetter` which reads an `Array`.

#### `prop`

``` purescript
prop :: String -> PartialGetter Foreign Foreign
```

A `PartialGetter` which reads an object property.

#### `index`

``` purescript
index :: Int -> PartialGetter Foreign Foreign
```

A `PartialGetter` which reads an array index.

#### `keys`

``` purescript
keys :: PartialGetter (Array String) Foreign
```

A `PartialGetter` which reads object keys.


