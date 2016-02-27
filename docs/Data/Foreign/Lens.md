## Module Data.Foreign.Lens

A `lens`-compatible layer for `purescript-foreign`.

#### `json`

``` purescript
json :: forall r. (Monoid r) => FoldP r String Foreign
```

A `forall r. FoldP r` which parses JSON.

#### `string`

``` purescript
string :: forall r. (Monoid r) => FoldP r Foreign String
```

A `forall r. FoldP r` which reads a `String`.

#### `char`

``` purescript
char :: forall r. (Monoid r) => FoldP r Foreign Char
```

A `forall r. FoldP r` which reads a `Char`.

#### `boolean`

``` purescript
boolean :: forall r. (Monoid r) => FoldP r Foreign Boolean
```

A `forall r. FoldP r` which reads a `Boolean`.

#### `number`

``` purescript
number :: forall r. (Monoid r) => FoldP r Foreign Number
```

A `forall r. FoldP r` which reads a `Number`.

#### `int`

``` purescript
int :: forall r. (Monoid r) => FoldP r Foreign Int
```

A `forall r. FoldP r` which reads an `Int`.

#### `array`

``` purescript
array :: forall r. (Monoid r) => FoldP r Foreign (Array Foreign)
```

A `forall r. FoldP r` which reads an `Array`.

#### `prop`

``` purescript
prop :: forall r. (Monoid r) => String -> FoldP r Foreign Foreign
```

A `forall r. FoldP r` which reads an object property.

#### `index`

``` purescript
index :: forall r. (Monoid r) => Int -> FoldP r Foreign Foreign
```

A `forall r. FoldP r` which reads an array index.

#### `keys`

``` purescript
keys :: forall r. (Monoid r) => FoldP r Foreign (Array String)
```

A `forall r. FoldP r` which reads object keys.


