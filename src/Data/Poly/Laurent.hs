-- |
-- Module:      Data.Poly.Laurent
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>.
--
-- @since 0.4.0.0
--

{-# LANGUAGE PatternSynonyms            #-}

module Data.Poly.Laurent
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

import Data.Poly.Internal.Dense.Laurent
