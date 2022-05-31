# poly [![Hackage](https://img.shields.io/hackage/v/poly.svg)](https://hackage.haskell.org/package/poly) [![Stackage LTS](https://www.stackage.org/package/poly/badge/lts)](https://www.stackage.org/lts/package/poly) [![Stackage Nightly](https://www.stackage.org/package/poly/badge/nightly)](https://www.stackage.org/nightly/package/poly) [![Coverage Status](https://coveralls.io/repos/github/Bodigrim/poly/badge.svg)](https://coveralls.io/github/Bodigrim/poly)

Haskell library for univariate and multivariate polynomials, backed by `Vector`s.

```haskell
> -- Univariate polynomials
> (X + 1) + (X - 1) :: VPoly Integer
2 * X
> (X + 1) * (X - 1) :: UPoly Int
1 * X^2 + (-1)

> -- Multivariate polynomials
> (X + Y) * (X - Y) :: VMultiPoly 2 Integer
1 * X^2 + (-1) * Y^2
> (X + Y + Z) ^ 2 :: UMultiPoly 3 Int
1 * X^2 + 2 * X * Y + 2 * X * Z + 1 * Y^2 + 2 * Y * Z + 1 * Z^2

> -- Laurent polynomials
> (X^-2 + 1) * (X - X^-1) :: VLaurent Integer
1 * X + (-1) * X^-3
> (X^-1 + Y) * (X + Y^-1) :: UMultiLaurent 2 Int
1 * X * Y + 2 + 1 * X^-1 * Y^-1
```

## Vectors

`Poly v a` is polymorphic over a container `v`, implementing the `Vector` interface, and coefficients of type `a`. Usually `v` is either a boxed vector from [`Data.Vector`](https://hackage.haskell.org/package/vector/docs/Data-Vector.html) or an unboxed vector from [`Data.Vector.Unboxed`](https://hackage.haskell.org/package/vector/docs/Data-Vector-Unboxed.html). Use unboxed vectors whenever possible, e. g., when the coefficients are `Int`s or `Double`s.

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

(Unfortunately, types are often ambiguous and must be given explicitly.)

While being convenient to read and write in REPL, `X` is relatively slow. The fastest approach is to use `toPoly`, providing it with a vector of coefficients (constant term first):

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

One can also find it convenient to `scale` by a monomial (cf. `monomial` above):

```haskell
> scale 2 3.5 (X^2 + 1) :: UPoly Double
3.5 * X^4 + 0.0 * X^3 + 3.5 * X^2 + 0.0 * X + 0.0
```

While `Poly` cannot be made an instance of `Integral` (because there is no meaningful `toInteger`),
it is an instance of `GcdDomain` and `Euclidean` from the [`semirings`](https://hackage.haskell.org/package/semirings) package. These type classes
cover the main functionality of `Integral`, providing division with remainder and `gcd` / `lcm`:

```haskell
> Data.Euclidean.gcd (X^2 + 7 * X + 6) (X^2 - 5 * X - 6) :: UPoly Int
1 * X + 1

> Data.Euclidean.quotRem (X^3 + 2) (X^2 - 1 :: UPoly Double)
(1.0 * X + 0.0,1.0 * X + 2.0)
```

Miscellaneous utilities include `eval` for evaluation at a given point,
and `deriv` / `integral` for taking the derivative and an indefinite integral, respectively:

```haskell
> eval (X^2 + 1 :: UPoly Int) 3
10

> deriv (X^3 + 3 * X) :: UPoly Double
3.0 * X^2 + 0.0 * X + 3.0

> integral (3 * X^2 + 3) :: UPoly Double
1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
```

## Deconstruction

Use `unPoly` to deconstruct a polynomial to a vector of coefficients (constant term first):

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

* `Data.Poly` provides dense univariate polynomials with a `Num`-based interface.
  This is a default choice for most users.

* `Data.Poly.Semiring` provides dense univariate polynomials with a `Semiring`-based interface.

* `Data.Poly.Laurent` provides dense univariate Laurent polynomials with a `Semiring`-based interface.

* `Data.Poly.Sparse` provides sparse univariate polynomials with a `Num`-based interface.
  Besides that, you may find it easier to use in the REPL
  because of a more readable `Show` instance, skipping zero coefficients.

* `Data.Poly.Sparse.Semiring` provides sparse univariate polynomials with a `Semiring`-based interface.

* `Data.Poly.Sparse.Laurent` provides sparse univariate Laurent polynomials with a `Semiring`-based interface.

* `Data.Poly.Multi` provides sparse multivariate polynomials with a `Num`-based interface.

* `Data.Poly.Multi.Semiring` provides sparse multivariate polynomials with a `Semiring`-based interface.

* `Data.Poly.Multi.Laurent` provides sparse multivariate Laurent polynomials with a `Semiring`-based interface.

All flavours are available backed by boxed or unboxed vectors.

## Performance

As a rough guide, `poly` is at least 20x-40x faster than the [`polynomial`](http://hackage.haskell.org/package/polynomial) library.
Multiplication is implemented via the Karatsuba algorithm.
Here are a couple of benchmarks for `UPoly Int`:

| Benchmark                     | polynomial, μs  | poly, μs | speedup
| :---------------------------- | --------------: | -------: | ------:
| addition, 100 coeffs.         |              45 |       2  |  22x
| addition, 1000 coeffs.        |             441 |      17  |  25x
| addition, 10000 coeffs.       |            6545 |     167  |  39x
| multiplication, 100 coeffs.   |            1733 |      33  |  52x
| multiplication, 1000 coeffs.  |          442000 |    1456  | 303x

## Additional resources

* __Polynomials in Haskell__, MuniHac, 12.09.2020:
  [slides](https://github.com/Bodigrim/my-talks/raw/master/munihac2020/slides.pdf),
  [video](https://youtu.be/NAs3ExQZUjA).
