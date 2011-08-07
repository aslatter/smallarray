{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types, BangPatterns #-}

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
module Data.SmallArray.Internal where

import Prelude hiding (length, elem)
import qualified Prelude

import Control.Exception (assert)
import Control.Monad.ST

import qualified Data.ByteArray as B
import Data.ByteArray (ByteArray, MutableByteArray)

import Data.Int
import Data.Word

import Control.DeepSeq
import qualified Data.Hashable as H

-- | A simple array. Indexing starts from zero.
newtype Array a
    = A ByteArray

-- | A simple mutable array. Indexing starts from zero.
newtype MArray s a
    = M (MutableByteArray s)

instance NFData (Array a) where
    rnf (A ary) = rnf ary

instance NFData (MArray s a) where
    rnf (M ary) = rnf ary

instance (Show e, Elt e) => Show (Array e) where
    show = show . toList

instance (Eq a, Elt a) => Eq (Array a) where
    {-# SPECIALIZE instance Eq (Array Word8) #-}
    (==) = eqArray

instance H.Hashable (Array a) where
    hash (A bArr) = B.hashByteArray bArr
    {-# INLINE hash #-}

eqArray :: (Eq a, Elt a) => Array a -> Array a -> Bool
eqArray a b
        = let an = length a
              bn = length b
          in
            an == bn && and [unsafeIndex a n == unsafeIndex b n | n <- [0..bn-1] ]
{-# INLINE eqArray #-}

instance (Ord a, Elt a) => Ord (Array a) where
    {-# SPECIALIZE instance Ord (Array Word8) #-}
    a `compare` b
        = f 0

     where an = length a
           bn = length b
           maxLen = an `min` bn

           f n | n < maxLen
                   = case unsafeIndex a n `compare` unsafeIndex b n of
                       EQ -> f (n+1)
                       x  -> x
               | an > bn
                   = GT
               | bn > an
                   = LT
               | otherwise
                   = EQ
    {-# INLINE compare #-}

class IArray a where
    -- | Return the length of an array.
    length :: a -> Int
    {-# INLINE length #-}

instance Elt a => IArray (Array a) where
    length = arrayLen

arrayLen :: Elt a => Array a -> Int
arrayLen a@(A arr)
    = len undefined a arr
 where
   len :: Elt e => e -> Array e -> ByteArray -> Int
   len elem _ bytes = B.length bytes `div` elemSize elem
{-# INLINE arrayLen #-}

instance Elt a => IArray (MArray s a) where
    length = marrayLen

marrayLen :: Elt a => MArray s a -> Int
marrayLen a@(M arr)
    = len undefined a arr
 where
   len :: Elt e => e -> MArray s e -> MutableByteArray s -> Int
   len elem _ bytes = B.lengthM bytes `div` elemSize elem
{-# INLINE marrayLen #-}

-- | Create a new array. The contents are not initialized in any way, and
-- may be invalid.
unsafeNew :: Elt e => Int -> ST s (MArray s e)
unsafeNew n = f undefined
   where f :: Elt e => e -> ST s (MArray s e)
         f e = M `fmap` B.new (bytesInArray n e)
{-# INLINE unsafeNew #-}

-- | Create a new array with the specified default value.
new :: Elt e => Int -> e -> ST s (MArray s e)
new n e = do
  arr <- unsafeNew n
  mapM_ (flip (unsafeWrite arr) e) [0 .. (n-1)]
  return arr

unsafeFreeze :: MArray s e -> ST s (Array e)
unsafeFreeze (M marr) = A `fmap` B.unsafeFreeze marr 
{-# INLINE unsafeFreeze #-}

-- | Execute an action creating a mutable array, and return the resulting
-- equivalent pure array. No copy is performed.
run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

run' :: (forall s . ST s (MArray s e, a)) -> (Array e, a)
run' act = runST $ do
             (marr, a) <- act
             arr <- unsafeFreeze marr
             return (arr, a)

-- | The empty array
empty :: Elt e => Array e
empty = run $ unsafeNew 0

-- | Output the array to a list.
toList :: Elt e => Array e -> [e]
toList arr = [arr `unsafeIndex` n | n <- [0 .. length arr - 1]]
{-# INLINE toList #-}

-- | Create an array from a list.
fromList :: Elt e => [e] -> Array e
fromList xs
    = run $ do
        arr <- unsafeNew len
        mapM_ (uncurry $ unsafeWrite arr) $ zip [0..(len-1)] xs
        return arr
 where len = Prelude.length xs
{-# INLINE fromList #-}

-- | Copy an array in its entirety. The destination array must be at
-- least as big as the source.
copy :: Elt e => MArray s e     -- ^ source array
     -> MArray s e              -- ^ destination array
     -> ST s ()
copy src dest
    | length dest >= length src = copy_loop 0
    | otherwise                 = fail "Data.SmallArray.copy: array too small"
    where
      len = length src
      copy_loop i
          | i >= len  = return ()
          | otherwise = do unsafeRead src i >>= unsafeWrite dest i
                           copy_loop (i+1)
{-# INLINE copy #-}

-- | Unsafely copy the elements of an array. Array bounds are not checked.
unsafeCopy :: Elt e =>
              MArray s e -> Int -> MArray s e -> Int -> Int -> ST s ()
unsafeCopy src sidx dest didx count =
    assert (sidx + count <= length src) .
    assert (didx + count <= length dest) $
    copy_loop sidx didx 0
    where
      copy_loop !i !j !c
          | c >= count  = return ()
          | otherwise = do unsafeRead src i >>= unsafeWrite dest j
                           copy_loop (i+1) (j+1) (c+1)
{-# INLINE unsafeCopy #-}


class Elt e where
    -- |Retrieve an element in an array at the specified
    -- location. Array indices start at zero.
    index :: Array e -> Int -> e
    index a n = check "index" a n unsafeIndex
    {-# INLINE index #-}

    -- |Retrieve an element from a mutable array at the
    -- specified location. Array indices start at zero.
    read :: MArray s e -> Int -> ST s e
    read a n = check "read" a n unsafeRead
    {-# INLINE read #-}

    -- |Write an element to a mutable array at
    -- the specified location. Array indices start
    -- at zero.
    write :: MArray s e -> Int -> e -> ST s ()
    write a n = check "write" a n unsafeWrite
    {-# INLINE write #-}

    -- | Size of the element in bytes
    elemSize :: e -> Int

    unsafeIndex :: Array e -> Int -> e
    unsafeRead :: MArray s e -> Int -> ST s e
    unsafeWrite :: MArray s e -> Int -> e -> ST s ()

bytesInArray :: Elt e => Int -> e -> Int
bytesInArray sz el = elemSize el * sz

check :: IArray a => String -> a -> Int -> (a -> Int -> b) -> b
check func ary i f
    | i >= 0 && i < length ary = f ary i
    | otherwise = error ("Data.SmallArray." ++ func ++ ": index out of bounds")
{-# INLINE check #-}

#define deriveElt(Typ) \
instance Elt Typ where { \
   elemSize = B.elemSize                     \
;  {-# INLINE elemSize #-} \
;  unsafeIndex (A arr) n   = B.index arr n    \
;  {-# INLINE unsafeIndex #-} \
;  unsafeRead  (M arr) n   = B.read  arr n    \
;  {-# INLINE unsafeRead #-} \
;  unsafeWrite (M arr) n x = B.write arr n x  \
;  {-# INLINE unsafeWrite #-} \
} \

deriveElt(Char)
deriveElt(Double)
deriveElt(Float)
deriveElt(Int)
deriveElt(Int8)
deriveElt(Int16)
deriveElt(Int32)
deriveElt(Int64)
deriveElt(Word)
deriveElt(Word8)
deriveElt(Word16)
deriveElt(Word32)
deriveElt(Word64)
