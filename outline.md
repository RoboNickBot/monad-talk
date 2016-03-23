# Functional power-levels

This talk will attempt to explain the usefulness of the `Functor`,
`Applicative`, and `Monad` typeclasses.

I will use two concrete instances (`Maybe a` and `[a]`) and show
specific difficulties with them that can be solved by stepping up to
the next typeclass on the ladder.

---

## `Maybe a`

Start with `Maybe`, make a little program with `Map` lookups

    lookup :: Map k v -> k -> Maybe v

Program will be ugly and complicated because it does not use any of
the following typeclass functions

---

### `instance Functor Maybe where...`

* Remind everyone what we can do using the `Functor` instance for
  `Maybe`
* Use `fmap` to factor code, reducing complexity

---

### `instance Applicative Maybe where...`

* Try to do something that requires `Applicative`, show that our
  `Functor` abilities break down
* Write extra code to do the thing, then show that it's the
  implementation for applicative

---

### `instance Monad Maybe where...`

* Now try to do something Monadic
* Chaining together lookups using `<*>` produces ugly, nested `Maybe`
  values
* Show that one layer of `Maybe` is what we really want
* Show how sequential `a -> Maybe a` operations can be combined
* It's the implementation for `Monad`!

---

## A sliding scale of power

    (<$>) :: Functor t     =>   (a -> b) -> (t a -> t b)
    (<*>) :: Applicative t => t (a -> b) -> (t a -> t b)
    (=<<) :: Monad t       => (a -> t b) -> (t a -> t b)

(from the [Haskell wikibook][1])

* Point out that `=<<` is the only operation that interacts with the
  surrounding "context" (`t`)
* `Applicative` gives an extra ability over `Functor`, but ability to
  edit the context gives `Monad` the most power
* Explain (or try to...) what I mean by a "value in a context"
    * The context of `Maybe a`: "This value may or may not exist"

* point out that all `Monad`s are `Applicative`s and all
  `Applicative`s are `Functor`s

---

## `[a]` is another `Monad`

* The context of `[a]`: "There are many alternate possibilites for
  this value"
* `fmap` is easy; it's just `map`

* `<*>`
    * think: a list of functions being applied to a list of values
    * implementation applies every function to every value, producing
      a list of all results
    * it can be thought of as a branching series of alternate
      realities!
    * show implementation

* `=<<`
    * think: applying a function *that produces a list* to a list of
      values
    * show implementation

---

## `do` notation? if there's time?

* show how do notation can make common patterns more readable

---

## Final thoughts

* `Monad` (and `Applicative`!) is very useful, but hard to wrap the
  mind around.
* Understanding what `Monad` does *in general* is more difficult and
  less important than understanding *specific* instances `Monad`, such
  as `[a]` and `Maybe a`
* Understanding and using many different instances is the best way to
  understand `Monad` in general

[1]: https://en.wikibooks.org/wiki/Haskell/Applicative_functors#A_sliding_scale_of_power
