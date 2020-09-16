-- |
-- Module:      Data.Poly.Multi
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse multivariate polynomials with 'Num' instance.
--

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Data.Poly.Multi
  ( MultiPoly
  , VMultiPoly
  , UMultiPoly
  , unMultiPoly
  , toMultiPoly
  , monomial
  , scale
  , pattern X
  , pattern Y
  , pattern Z
  , eval
  , subst
  , deriv
  , integral
  , segregate
  , unsegregate
  ) where

import Data.Poly.Internal.Multi
import Data.Poly.Internal.Multi.Field ()
import Data.Poly.Internal.Multi.GcdDomain ()
