**Introduction**

Bisquit is a programming language.


**Getting started**

Install sbt (https://www.scala-sbt.org/1.x/docs/Setup.html) and run `sbt run`
to start Bisquit's repl.

Bisquit is a statically typed language and has a module system. Out of the box,
it doesn't come with much but you can import the `Prelude` module to get access
to helpful functions. Below is an example showing how to do this along with a
successful result and a type error.

```
> import Prelude exposing (...)
< ok

> +
= <fn> : a -> a -> a where a < Num

> +(3, 45)
= 48 : Int

> +(3, "one")
type error: given a value of type Str where a value of Int was expected:

  <repl>:1 | +(3, "one")
                  ^

>
```

Here's an example showing off the structural typing of record types and a
recursive function definition with full type inference.

```
> def len(obj) =
    if nil?(obj.list)
    then 0
    else +(1, len(cdr(obj.list)))
: { list : List[a] } -> Int

> len({ list = [1, 2, 3] })
= 3 : Int
```

Bindings are immutable by default. There is variable shadowing, but once a
binding is set and used, it is static. Reference cells provide mutability when
you need it:

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
