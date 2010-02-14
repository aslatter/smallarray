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

import Prelude hiding (length)
import qualified Prelude

import Control.Exception (assert)
import Control.Monad.ST

import qualified Data.ByteArray as B
import Data.ByteArray (ByteArray, MutableByteArray)

import Data.Int
import Data.Word

import Control.DeepSeq

data Array a
    = A {-# UNPACK #-}!Int
        {-# UNPACK #-}!ByteArray
data MArray s a
    = M {-# UNPACK #-}!Int
        {-# UNPACK #-}!(MutableByteArray s)

instance NFData (Array a) where
    rnf (A _ ary) = rnf ary

instance NFData (MArray s a) where
    rnf (M _ ary) = rnf ary

instance (Show e, Elt e) => Show (Array e) where
    show = show . toList

instance (Eq a, Elt a) => Eq (Array a) where
    (==) = eqArray

eqArray :: (Eq a, Elt a) => Array a -> Array a -> Bool
eqArray a@(A an _) b@(A bn _)
        = an == bn && and [unsafeIndex a n == unsafeIndex b n | n <- [0..bn-1] ]
{-# INLINE eqArray #-}

instance (Ord a, Elt a) => Ord (Array a) where
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

class IArray a where
    length :: a -> Int
    {-# INLINE length #-}

instance IArray (Array a) where
    length = arrayLen

arrayLen :: Array a -> Int
arrayLen (A n _) = n
{-# INLINE arrayLen #-}

instance IArray (MArray s a) where
    length = marrayLen

marrayLen :: MArray s a -> Int
marrayLen (M n _) = n
{-# INLINE marrayLen #-}

unsafeNew :: Elt e => Int -> ST s (MArray s e)
unsafeNew n = f undefined
   where f :: Elt e => e -> ST s (MArray s e)
         f e = M n `fmap` B.new (bytesInArray n e)
{-# INLINE unsafeNew #-}

new :: Elt e => Int -> e -> ST s (MArray s e)
new n e = do
  arr <- unsafeNew n
  mapM_ (flip (unsafeWrite arr) e) [0 .. (n-1)]
  return arr

unsafeFreeze :: MArray s e -> ST s (Array e)
unsafeFreeze (M n marr) = A n `fmap` B.unsafeFreeze marr 
{-# INLINE unsafeFreeze #-}

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

run2 :: (forall s . ST s (MArray s e, a)) -> (Array e, a)
run2 act = runST $ do
             (marr, a) <- act
             arr <- unsafeFreeze marr
             return (arr, a)

empty :: Elt e => Array e
empty = run $ unsafeNew 0

toList :: Elt e => Array e -> [e]
toList arr = [arr `unsafeIndex` n | n <- [0 .. length arr - 1]]
{-# INLINE toList #-}

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
    | otherwise                 = fail "Data.Text.Array.copy: array too small"
    where
      len = length src
      copy_loop i
          | i >= len  = return ()
          | otherwise = do unsafeRead src i >>= unsafeWrite dest i
                           copy_loop (i+1)
{-# INLINE copy #-}

-- | Unsafely copy the elements of an array.
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

    bytesInArray :: Int -> e -> Int
    unsafeIndex :: Array e -> Int -> e
    unsafeRead :: MArray s e -> Int -> ST s e
    unsafeWrite :: MArray s e -> Int -> e -> ST s ()

check :: IArray a => String -> a -> Int -> (a -> Int -> b) -> b
check func ary i f
    | i >= 0 && i < length ary = f ary i
    | otherwise = error ("Data.SmallArray." ++ func ++ ": index out of bounds")
{-# INLINE check #-}

#define deriveElt(Typ) \
instance Elt Typ where { \
   bytesInArray sz el = B.elemSize el * sz      \
;  {-# INLINE bytesInArray #-} \
;  unsafeIndex (A _ arr) n   = B.index arr n    \
;  {-# INLINE unsafeIndex #-} \
;  unsafeRead  (M _ arr) n   = B.read  arr n    \
;  {-# INLINE unsafeRead #-} \
;  unsafeWrite (M _ arr) n x = B.write arr n x  \
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
