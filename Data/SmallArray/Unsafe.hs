{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Data.SmallArray.Unsafe
    (
      Elt(unsafeRead, unsafeWrite, unsafeIndex)
    , unsafeNew
    , unsafeFreeze
    ) where

import Data.SmallArray.Internal