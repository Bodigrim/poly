# poly [![Build Status](https://travis-ci.org/Bodigrim/poly.svg)](https://travis-ci.org/Bodigrim/poly) [![Hackage](http://img.shields.io/hackage/v/poly.svg)](https://hackage.haskell.org/package/poly)

Univariate polynomials with `Num` and `Semiring` instances, backed by `Vector`.

```haskell
> (X + 1) + (X - 1) :: VPoly Integer
2 * X + 0

> (X + 1) * (X - 1) :: UPoly Int
1 * X^2 + 0 * X + (-1)

> eval (X^2 + 1 :: UPoly Int) 3
10

> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
1 * X^2 + 2 * X + 2

> deriv (X^3 + 3 * X) :: UPoly Int
3 * X^2 + 0 * X + 3
```

## Vectors

Polynomial `Poly v a` is polymorhic over a container `v`, implementing `Vector` interface, and coefficients of type `a`. Usually `v` is either a boxed vector from `Data.Vector` or an unboxed vector from `Data.Vector.Unboxed`. Use unboxed vectors whenever possible, e. g., when coefficients are `Int` or `Double`.

There are handy type synonyms:

```haskell
type VPoly a = Poly Data.Vector.Vector         a
type UPoly a = Poly Data.Vector.Unboxed.Vector a
```

## Construction

The simplest way to construct a polynomial is using the pattern `X`:

```haskell
> X^2 - 3*X + 2 :: UPoly Int
1 * X^2 + (-3) * X + 2
```

(Unfortunately, a type is often ambiguous and must be explicit.)

While being convenient to read and use in REPL, `X` is relatively slow. The fastest approach is to use `toPoly`, providing it with a vector of coefficients (head is the constant term):

```haskell
> toPoly (Data.Vector.Unboxed.fromList [2, -3, 1 :: Int])
1 * X^2 + (-3) * X + 2
```

TODO note on `monomial`

## Operations

Most operations on `Poly` are provided by means of instances, like `Eq` and `Num`. For example,

```haskell
> (X^2 + 1) * (X^2 - 1) :: UPoly Int
1 * X^4 + 0 * X^3 + 0 * X^2 + 0 * X + (-1)
```

TODO eval, deriv, integral

TODO Euclidean and GcdDomain

## Deconstruction

Use `unPoly` to deconstruct a polynomial to a vector of coefficients:

```haskell
> unPoly (X^2 - 3*X + 2 :: UPoly Int)
[2,-3,1]
```

TODO note on `leading`

## Flavors

TODO Num/Semiring, Dense/Sparse
