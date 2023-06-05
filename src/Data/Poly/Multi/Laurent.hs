-- |
-- Module:      Data.Poly.Multi.Laurent
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse multivariate
-- <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>.
--

{-# LANGUAGE PatternSynonyms            #-}

module Data.Poly.Multi.Laurent
  ( MultiLaurent
  , VMultiLaurent
  , UMultiLaurent
  , Monom
  , unMultiLaurent
  , toMultiLaurent
  , monomial
  , scale
  , pattern X
  , pattern Y
  , pattern Z
  , (^-)
  , eval
  , subst
  , deriv
  , segregate
  , unsegregate
  ) where

import Data.Poly.Internal.Multi.Laurent
