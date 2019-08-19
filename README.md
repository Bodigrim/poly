# poly [![Build Status](https://travis-ci.org/Bodigrim/poly.svg)](https://travis-ci.org/Bodigrim/poly) [![Hackage](http://img.shields.io/hackage/v/poly.svg)](https://hackage.haskell.org/package/poly) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/poly/badge)](https://matrix.hackage.haskell.org/package/poly) [![Stackage LTS](http://stackage.org/package/poly/badge/lts)](http://stackage.org/lts/package/poly) [![Stackage Nightly](http://stackage.org/package/poly/badge/nightly)](http://stackage.org/nightly/package/poly)



Haskell library for univariate polynomials, backed by `Vector`.

```haskell
> (X + 1) + (X - 1) :: VPoly Integer
2 * X + 0

> (X + 1) * (X - 1) :: UPoly Int
1 * X^2 + 0 * X + (-1)
```

## Vectors

`Poly v a` is polymorphic over a container `v`, implementing `Vector` interface, and coefficients of type `a`. Usually `v` is either a boxed vector from `Data.Vector` or an unboxed vector from `Data.Vector.Unboxed`. Use unboxed vectors whenever possible, e. g., when coefficients are `Int` or `Double`.

There are handy type synonyms:

```haskell
type VPoly a = Poly Data.Vector.Vector         a
type UPoly a = Poly Data.Vector.Unboxed.Vector a
```

## Construction

The simplest way to construct a polynomial is using the pattern `X`:

```haskell
> X^2 - 3 * X + 2 :: UPoly Int
1 * X^2 + (-3) * X + 2
```

(Unfortunately, a type is often ambiguous and must be given explicitly.)

While being convenient to read and write in REPL, `X` is relatively slow. The fastest approach is to use `toPoly`, providing it with a vector of coefficients (head is the constant term):

```haskell
> toPoly (Data.Vector.Unboxed.fromList [2, -3, 1 :: Int])
1 * X^2 + (-3) * X + 2
```

Alternatively one can enable `{-# LANGUAGE OverloadedLists #-}` and simply write

```haskell
> [2, -3, 1] :: UPoly Int
1 * X^2 + (-3) * X + 2
```

There is a shortcut to construct a monomial:

```haskell
> monomial 2 3.5 :: UPoly Double
3.5 * X^2 + 0.0 * X + 0.0
```

## Operations

Most operations are provided by means of instances, like `Eq` and `Num`. For example,

```haskell
> (X^2 + 1) * (X^2 - 1) :: UPoly Int
1 * X^4 + 0 * X^3 + 0 * X^2 + 0 * X + (-1)
```

One can also find convenient to `scale` by monomial (cf. `monomial` above):

```haskell
> scale 2 3.5 (X^2 + 1) :: UPoly Double
3.5 * X^4 + 0.0 * X^3 + 3.5 * X^2 + 0.0 * X + 0.0
```

While `Poly` cannot be made an instance of `Integral` (because there is no meaningful `toInteger`),
it is an instance of `GcdDomain` and `Euclidean` from `semirings` package. These type classes
cover main functionality of `Integral`, providing division with remainder and `gcd` / `lcm`:

```haskell
> Data.Euclidean.gcd (X^2 + 7 * X + 6) (X^2 - 5 * X - 6) :: UPoly Int
1 * X + 1

> Data.Euclidean.quotRem (X^3 + 2) (X^2 - 1 :: UPoly Double)
(1.0 * X + 0.0,1.0 * X + 2.0)
```

Miscellaneous utilities include `eval` for evaluation at a given value of indeterminate,
and reciprocals `deriv` / `integral`:

```haskell
> eval (X^2 + 1 :: UPoly Int) 3
10

> deriv (X^3 + 3 * X) :: UPoly Double
3.0 * X^2 + 0.0 * X + 3.0

> integral (3 * X^2 + 3) :: UPoly Double
1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
```

## Deconstruction

Use `unPoly` to deconstruct a polynomial to a vector of coefficients (head is the constant term):

```haskell
> unPoly (X^2 - 3 * X + 2 :: UPoly Int)
[2,-3,1]
```

Further, `leading` is a shortcut to obtain the leading term of a non-zero polynomial,
expressed as a power and a coefficient:

```haskell
> leading (X^2 - 3 * X + 2 :: UPoly Double)
Just (2,1.0)
```

## Flavours

The same API is exposed in four flavours:

* `Data.Poly` provides dense polynomials with `Num`-based interface.
  This is a default choice for most users.

* `Data.Poly.Semiring` provides dense polynomials with `Semiring`-based interface.

* `Data.Poly.Sparse` provides sparse polynomials with `Num`-based interface.
  Besides that, you may find it easier to use in REPL
  because of a more readable `Show` instance, skipping zero coefficients.

* `Data.Poly.Sparse.Semiring` provides sparse polynomials with `Semiring`-based interface.

All flavours are available backed by boxed or unboxed vectors.

## Performance

As a rough guide, `poly` is at least 20x-40x faster than [`polynomial`](http://hackage.haskell.org/package/polynomial) library.
Multiplication is implemented via Karatsuba algorithm.
Here is a couple of benchmarks for `UPoly Int`.

| Benchmark                     | polynomial, μs  | poly, μs | speedup
| :---------------------------- | --------------: | -------: | ------:
| addition, 100 coeffs.         |              45 |       2  |  22x
| addition, 1000 coeffs.        |             441 |      17  |  25x
| addition, 10000 coeffs.       |            6545 |     167  |  39x
| multiplication, 100 coeffs.   |            1733 |      33  |  52x
| multiplication, 1000 coeffs.  |          442000 |    1456  | 303x
