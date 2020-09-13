-- |
-- Module:      Data.Poly.Sparse.Laurent
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>.
--

{-# LANGUAGE PatternSynonyms #-}

module Data.Poly.Sparse.Laurent
  ( Laurent
  , VLaurent
  , ULaurent
  , unLaurent
  , toLaurent
  , leading
  , monomial
  , scale
  , pattern X
  , (^-)
  , eval
  , subst
  , deriv
  ) where

import Data.Poly.Internal.Sparse.Laurent
