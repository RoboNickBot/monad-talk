# Functional power-levels

Operating on values using the `Functor`, `Applicative`, and `Monad`
typeclasses

---

## `Functor`

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

`fmap` promotes an ordinary function `(a -> b)` to a function that
operates on values in a context `(f a -> f b)`.

    map :: ( a -> b ) -> ( [a] -> [b] )

The `fmap` instance for `Either e` works just like that of `Maybe`

    instance Functor Either e where
      fmap f (Right r) = Right (f r)
      fmap _ (Left e) = Left e

`fmap` also has a useful infix form, `<$>`

    fst <$> Just ("a","b") = Just "a"

---

## Values in a context

A "context" means some extra information or effects surrounding a
otherwise ordinary value

* `Double` indicates a floating-point number (no context)
* `[Double]` is a floating-point number that may have many (or zero)
  values
* `Maybe Double` is a floating-point number that may or may not exist
* `Writer String Double` is a floating-point number that may have had
  notes (of type `String`) attached to it by previous computations

* `Functor`, `Applicative`, and `Monad` all model values in context
* the context becomes more malleable as we slide up the scale

---

## Currying

Haskell functions technically only ever take one argument. (remember
that functions are first-class!)

    foo :: a -> b -> c -> d
    == foo :: a -> ( b -> c -> d )
    == foo :: a -> ( b -> ( c -> d )

    bar :: c -> d
    bar = foo a1 b1

---

## `Applicative`

    class (Functor f) => Applicative f where
      pure  :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

`Applicative` is *more powerful* than `Functor`.

    (<$>) :: (a -> b) -> f a -> f b
    f <$> a = pure f <*> a

Currying allows us to perform full function application even in the
presence of contextual values.

    -- (+) :: Int -> Int -> Int

    -- pure (+) :: Maybe Int -> Maybe Int -> Maybe Int

    ( (+) <$> Just 3 <*> Just 4 ) == Just 7

With `Applicative`, we can pretend the context doesn't exist whenever
we want!

---

## `instance Applicative (Either e)`

`pure` simply wraps a value (often a function) in a "successful"
context

    pure :: a -> f a
    pure a = Right a

`<*>` passes any errors down the line, favoring the lefthand
("earlier") side if there are two failures

    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (Right f) <*> (Right a) = Right (f a)
    (Left e)  <*> _         = Left e
    _         <*> (Left e)  = Left e

---

## `Monad`

    class (Applicative m) => Monad m where
      (>>=)  :: m a -> (a -> m b) -> m b
      (>>)   :: m a -> m b -> m b
      return :: a -> m a
      fail   :: String -> m a

`Monad` is more powerful still, allowing us to choose the *context* of
a contextual value based on an ordinary value.

"Monadic" functions `(a -> m b)` are tricky to deal with.  Applying
them with `fmap` just makes big nested types.

    foo :: a -> m b
    bar :: m a
    
    fmap foo bar :: m (m b) -- whoops?

---

## Bind

    (>>=)  :: m a -> (a -> m b) -> m b

The typeclass gives us `>>=` ("bind"), which chains "Monadic"
functions together.

    foo :: a -> b
    bar :: b -> b
    can :: b -> c

    can . bar . foo :: a -> c

The reverse operator `=<<` has a clear relationship with `.`

    fooM :: a -> m b
    barM :: b -> m b
    canM :: b -> m c

    canM =<< barM =<< fooM :: a -> m c

Contexts are combined in sequence, avoiding context nesting

---

## `instance Monad (Either e) where`

    (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    (Right a) >>= f = f a
    (Left e)  >>= _ = Left e

There's nothing to combine, either an error is passed on or it isn't.

    return :: a -> Either e a
    return = Right

`return` is identical to `pure`

---

## The sliding scale of power

    (<$>) :: Functor t     =>   (a -> b) -> (t a -> t b)
    (<*>) :: Applicative t => t (a -> b) -> (t a -> t b)
    (=<<) :: Monad t       => (a -> t b) -> (t a -> t b)

(got this beautiful comparison from the [Haskell wikibook][1])

Armed with these functions, `=<<` is the only one which allows us to
*create* contexts `(a -> t b)` with our inputs.

`Applicative` allows only the combining of contexts we're bringing along.

`Functor` allows no effects to the context *at all*.

The less we can do, the easier our code is to think about.

---

## Lists

The main "context" assigned to `[a]` is that of a non-deterministic value.

A function `a -> [b]` produces any number of "alternate possibilities"
for b, and our typeclasses allow us to work with them without hassel.

    instance Functor [] where
      fmap = map

`Functor` is easy, but things get tricky when we have to *combine*
non-determinism for `Applicative`

    instance Applicative [] where
      pure a = [a]
      fs <*> as  = [ f a | f <- fs, a <- as ]

This instance takes every function in the list on the left and applies
each one to every value in the list on the right!

    instance Monad [] where
      (>>=) [a] -> (a -> [b]) -> [b]
      as >>= f = concat (map f as)
      ...

---

## Final thoughts

* `Monad` and its superclasses are very useful, but hard to wrap the
  mind around.
* Understanding what `Monad` does *in general* is more difficult and
  less important than understanding *specific* instances `Monad`, such
  as `[a]` and `Either e`
* Understanding and using many different instances is the best way to
  understand `Monad` in general

### Resources

* The code and slides (in Markdown form) from this talk are on GitHub
    * https://github.com/RoboNickBot/monad-talk

* The Haskell wikibook has some great chapters on these typeclasses,
  which I used as guides for this talk
    * https://en.wikibooks.org/wiki/Haskell
    * https://en.wikibooks.org/wiki/Haskell/The_Functor_class
    * https://en.wikibooks.org/wiki/Haskell/Applicative_functors
    * https://en.wikibooks.org/wiki/Haskell/Understanding_monads

[1]: https://en.wikibooks.org/wiki/Haskell/Applicative_functors#A_sliding_scale_of_power
