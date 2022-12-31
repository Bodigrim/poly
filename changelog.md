# 0.5.1.0

* Add function `timesRing`.

# 0.5.0.0

* Change definition of `Data.Euclidean.degree`
  to coincide with the degree of polynomial.
* Implement multivariate polynomials (usual and Laurent).
* Reimplement sparse univariate polynomials as a special case of multivariate ones.
* Speed up `gcd` calculations for all flavours of polynomials.
* Decomission `PolyOverField` and `LaurentOverField`: they do not improve performance any more.
* Add function `quotRemFractional`.
* Add an experimental implementation of the discrete Fourier transform.
* Add conversion functions between dense and sparse polynomials.

# 0.4.0.0

* Implement Laurent polynomials.
* Implement orthogonal polynomials.
* Decomission extended GCD, use `Data.Euclidean.gcdExt`.
* Decomission `PolyOverFractional`, use `PolyOverField`.

# 0.3.3.0

* Add function `subst`.
* Fix compatibility issues.

# 0.3.2.0

* Add `NFData` instance.
* Implement extended GCD.
* Rename `PolyOverFractional` to `PolyOverField`.
* Add `integral` with `Semiring`-based interface.

# 0.3.1.0

* Implement Karatsuba multiplication.
* Add `IsList` instance.

# 0.3.0.0

* Implement sparse polynomials.
* Add `GcdDomain` and `Euclidean` instances.
* Add functions `leading`, `monomial`, `scale`.
* Remove function `constant`.

# 0.2.0.0

* Parametrize `Poly` by underlying vector type.
* Introduce `Data.Poly.Semiring` module.
* Fix a bug in `Num.(-)`.
* Add functions `constant`, `eval`, `deriv`, `integral`.
* Add a handy pattern synonym `X`.
* Add type synonyms `VPoly` and `UPoly`.
* Remove function `toPoly'`.

# 0.1.0.0

* Initial release.
