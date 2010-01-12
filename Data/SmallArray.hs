{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

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
    , fromList

    -- * Unpacking
    , toList
    ) where

import Data.SmallArray.Internal
