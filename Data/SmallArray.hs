-- |
-- Module      : Data.SmallArray
-- Copyright   : (c) Bryan O'Sullivan 2009
--               (c) Antoine Latter 2010
--
-- License     : BSD-style
-- Maintainer  : aslatter@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.SmallArray as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualifid
-- naming.
module Data.SmallArray
    (
    -- * Array types
      IArray(..)
    , Elt(read, write, index)
    , Array
    , MArray

    -- * Creation
    , empty
    , new
    , run
    , run2
    , fromList
    , copy

    -- * Unpacking
    , toList
    ) where

import Data.SmallArray.Internal
