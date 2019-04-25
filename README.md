# poly [![Build Status](https://travis-ci.org/Bodigrim/poly.svg)](https://travis-ci.org/Bodigrim/poly) [![Hackage](http://img.shields.io/hackage/v/poly.svg)](https://hackage.haskell.org/package/poly)

Polynomials with `Num` and `Semiring` instances, backed by `Vector`.

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
