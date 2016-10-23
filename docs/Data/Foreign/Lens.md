## Module Data.Foreign.Lens

A `lens`-compatible layer for `purescript-foreign`.

#### `json`

``` purescript
json :: forall r. Monoid r => Fold' r String Foreign
```

A `Fold` which parses JSON.

#### `string`

``` purescript
string :: forall r. Monoid r => Fold' r Foreign String
```

A `Fold` which reads a `String`.

#### `char`

``` purescript
char :: forall r. Monoid r => Fold' r Foreign Char
```

A `Fold` which reads a `Char`.

#### `boolean`

``` purescript
boolean :: forall r. Monoid r => Fold' r Foreign Boolean
```

A `Fold` which reads a `Boolean`.

#### `number`

``` purescript
number :: forall r. Monoid r => Fold' r Foreign Number
```

A `Fold` which reads a `Number`.

#### `int`

``` purescript
int :: forall r. Monoid r => Fold' r Foreign Int
```

A `Fold` which reads an `Int`.

#### `array`

``` purescript
array :: forall r. Monoid r => Fold' r Foreign (Array Foreign)
```

A `Fold` which reads an `Array`.

#### `prop`

``` purescript
prop :: forall r. Monoid r => String -> Fold' r Foreign Foreign
```

A `Fold` which reads an object property.

#### `index`

``` purescript
index :: forall r. Monoid r => Int -> Fold' r Foreign Foreign
```

A `Fold` which reads an array index.

#### `keys`

``` purescript
keys :: forall r. Monoid r => Fold' r Foreign (Array String)
```

A `Fold` which reads object keys.


