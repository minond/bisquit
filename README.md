# Bisquit

Bisquit is an exercise in building a statically typed functional programming
language with type inference. To get started, install `sbt`
(https://www.scala-sbt.org/1.x/docs/Setup.html) and run `sbt run`.

When you start the repl, all of the declarations from the
[`Prelude`](lib/Prelude.bisquit) module will be imported and exposed to the
scope. Doing this allows you to use all of the builtins which let you do
arithmetic, boolean logic, list manipulation, and ref cell manipulation.

```
> import Prelude exposing (...)
< ok
```

Bisquit's type system implements generics. Types can also be extended and used
as constraint in a generic type:

```
> +
= <fn> : a -> a -> a where a < Num

> eq
= <fn> : a -> b -> Bool where a < Ord, b < Ord
```

Note `+` wants two parameters of the same generic type, `a`, while `eq` accepts
parameters of different types, as long as they both extend `Ord`. We can see
what Bisquit does at compile time when this constraint is not met:

```
> +(3, 45)
= 48 : Int

> +(3, "one")
type error: given a value of type Str where a value of Int was expected:

  <repl>:1 | +(3, "one")
                  ^
```

Notice the half decent error message. Not bad huh? Now let's show how a `len`
function would be written for objects with a `list` property within it:

```
> def len(obj) =
    if nil?(obj.list)
    then 0
    else +(1, len(cdr(obj.list)))
: { list : List[a] } -> Int

> len({ list = [1, 2, 3] })
= 3 : Int
```

Bisquit has structural typing, so the inferred type of `len` ends up being `{
list : List[a] } -> Int`. A named type can be used as a constraint, but
otherwise only its shape will be used and checked by the typechecker.

The last example I'll show is that of a ref cell. All declarations are
immutable and are only shadowed when re-declared (meaning existing references
to the old value won't change), however ref cells provide you with an object
that you can mutate the "inner memory" of. They similar to [SML's
references](https://en.wikibooks.org/wiki/Standard_ML_Programming/Types#References).

Here's an example of a counter function that returns a value it increments
internally every time we call it:

```
> def make_counter(start) =
  let
    curr = ref!(start)
  in
    fn () = map!(curr, +(1))
: Int -> (Unit -> Int)
```

```
> def counter = make_counter(0)
: Unit -> Int

> counter()
= 1 : Int

> counter()
= 2 : Int

> counter()
= 3 : Int
```

There are a few other features implemented in Bisquit, and a few things that
are not working as I want them to yet, but the inference is in a _good enough_
place for a pet project like this one so this project likely won't see much
more work in the future.
