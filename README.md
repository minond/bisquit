**Introduction**

Bisquit is an exercise in building a statically typed functional programming
language with type inference. To get started, install sbt
(https://www.scala-sbt.org/1.x/docs/Setup.html) and run to start the repl. Most
of Bisquit's builtins can be found in the `Prelude` module. Below are a set of
examples showing off features of the language.


**Examples**

```
> import Prelude exposing (...)
< ok

> +
= <fn> : a -> a -> a where a < Num
```

```
> +(3, 45)
= 48 : Int

> +(3, "one")
type error: given a value of type Str where a value of Int was expected:

  <repl>:1 | +(3, "one")
                  ^
```

```
> def len(obj) =
    if nil?(obj.list)
    then 0
    else +(1, len(cdr(obj.list)))
: { list : List[a] } -> Int

> len({ list = [1, 2, 3] })
= 3 : Int
```

```
> def make_counter(start) =
  let
    curr = ref!(start)
  in
    fn () = map!(curr, +(1))
: Int -> (Unit -> Int)

> def counter = make_counter(0)
: Unit -> Int

> counter()
= 1 : Int

> counter()
= 2 : Int

> counter()
= 3 : Int
```
