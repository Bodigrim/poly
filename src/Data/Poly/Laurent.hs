-- |
-- Module:      Data.Poly.Laurent
-- Copyright:   (c) 2019 Alberto Centelles
-- Licence:     BSD3
-- Maintainer:  Alberto Centelles <centelles.alberto@gmail.com>
--
-- Laurent polynomials
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Poly.Laurent
  ( Laurent
  , VLaurent
  , ULaurent
  , toLaurent
  ) where

import           Data.Poly.Internal.Dense.Laurent

