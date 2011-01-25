{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

#ifndef TESTING_FFI
#if defined(__GLASGOW_HASKELL__)
#define USING_GHC
#endif
#endif

{-|

This is not a safe data structure - you can do very bad things, and array bounds are
not checked in any of these functions.

You can also create non-referentialy transparent structures quite easily.
So don't.

In summary: This is a portable, low-level type for building higher-level, safe types.

When compiling with GHC, the ByteArray type is a wrapper around GHC.Prim.ByteArray#.
Otherwise it's a thin wrapper arounf the FFI ForeignPtr.

You get the benifit of using the fastest unboxed primitive on your platform without
worrying about the details.

-}

module Data.ByteArray
    ( ByteArray
    , MutableByteArray
    , new
    , newPinned
    , length
    , lengthM
    , Elt(..)
    , unsafeFreeze
    , asPtr
    , hashByteArray
    ) where


#if defined(USING_GHC)
-- for GHC we use GHC.Prim byte arrays
import GHC.Exts
#if __GLASGOW_HASKELL__ > 610
import GHC.IO (IO(..))
#else
import GHC.IOBase (IO(..))
#endif
import GHC.ST (ST(..))

import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))

#include "MachDeps.h"

#else
import Foreign hiding (new)
#endif

import Prelude hiding (length)
import Control.Monad.ST
import Control.DeepSeq

import qualified Data.Hashable as H

#if defined(USING_GHC)
-- in GHC prior to 7, the built-in length primitive on ByteArrays would
-- round up to the nearest 4-byte word. After it returns the amount request
-- during allocation, which is what we want.
-- Prior to GHC 7 we carry around a length.
#if __GLASGOW_HASKELL__ > 612
#ifndef TESTING_LENGTH
#define HAS_LENGTH
#endif
#endif

#ifdef HAS_LENGTH
data ByteArray = ByteArray {unArray :: !ByteArray# }
data MutableByteArray s = MutableByteArray {unMArray :: !(MutableByteArray# s)}
#else
data ByteArray = ByteArray {unArray :: !ByteArray#, length :: {-# UNPACK #-}!Int }
data MutableByteArray s = MutableByteArray {unMArray :: !(MutableByteArray# s), lengthM :: {-# UNPACK #-}!Int}
#endif

#else
-- fallback to FFI foreign pointers
data ByteArray = ByteArray {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int
data MutableByteArray s = MutableByteArray {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int
#endif

instance NFData ByteArray where
instance NFData (MutableByteArray s) where

-- | Allocate a new array. The size is specified in bytes.
new :: Int -> ST s (MutableByteArray s)
{-# INLINE new #-}

-- | Allocate a new array in a memory region which will not
-- be moved. The size is specified in bytes.
newPinned :: Int -> ST s (MutableByteArray s)
{-# INLINE newPinned #-}

-- | Convert a MutableByteArray to a ByteArray. You should
-- not modify the source array after calling this.
unsafeFreeze :: MutableByteArray s -> ST s ByteArray
{-# INLINE unsafeFreeze #-}

length :: ByteArray -> Int
lengthM :: MutableByteArray s -> Int
{-# INLINE length #-}
{-# INLINE lengthM #-}

class Elt a where
    -- | Read a primitve element from a mutable byte array.
    -- The index is positon, not bytes.
    read     :: MutableByteArray s -> Int -> ST s a

    -- | Write to a location in a mutable byte array.
    -- The index is position, not bytes.
    write    :: MutableByteArray s -> Int -> a -> ST s ()

    -- | Read from a location in a byte array.
    -- The index is position, not bytes.
    index    :: ByteArray -> Int -> a

    -- | The size of an element in bytes.
    elemSize :: a -> Int

-- | Only for use with pinned arrays.
asPtr :: ByteArray -> (Ptr a -> IO b) -> IO b

#if defined(USING_GHC)

#ifdef HAS_LENGTH
new (I# n#)
    = ST $ \s -> case newByteArray# n# s of
                   (# s', ary #) -> (# s', MutableByteArray ary #)
newPinned (I# n#)
    = ST $ \s -> case newPinnedByteArray# n# s of
                   (# s', ary #) -> (# s', MutableByteArray ary #)

unsafeFreeze (MutableByteArray mary)
    = ST $ \s -> case unsafeFreezeByteArray# mary s of
                   (# s', ary #) -> (# s', ByteArray ary #)

length (ByteArray ary) = I# (sizeofByteArray# ary)
lengthM (MutableByteArray mary) = I# (sizeofMutableByteArray# mary)

#else
new n@(I# n#)
    = ST $ \s -> case newByteArray# n# s of
                   (# s', ary #) -> (# s', MutableByteArray ary n #)
newPinned n@(I# n#)
    = ST $ \s -> case newPinnedByteArray# n# s of
                   (# s', ary #) -> (# s', MutableByteArray ary n #)

unsafeFreeze (MutableByteArray mary n)
    = ST $ \s -> case unsafeFreezeByteArray# mary s of
                   (# s', ary #) -> (# s', ByteArray ary n #)
#endif

asPtr a k
    = case byteArrayContents# (unArray a) of
        addr# -> do
          x <- k $ Ptr addr#
          touch a
          return x

touch :: a -> IO ()
touch x = IO $ \s-> case touch# x s of s' -> (# s', () #)

hashByteArray arr = H.hashByteArray (unArray arr) 0 (length arr)

#define deriveElt(Typ, Ct, rd, wrt, ix, sz) \
instance Elt Typ where { \
    read ary (I# n) = ST (\s -> case rd (unMArray ary) n s of \
                                   {(# s', b #) -> (# s', Ct b #)}) \
;   {-# INLINE read #-} \
;   write ary (I# n) (Ct b) = ST (\s -> (# wrt (unMArray ary) n b s, () #)) \
;   {-# INLINE write #-} \
;   index ary (I# n) = Ct (ix (unArray ary) n) \
;   {-# INLINE index #-} \
;   elemSize _ = sz \
;   {-# INLINE elemSize #-} \
}


deriveElt(Word, W#, readWordArray#, writeWordArray#, indexWordArray#, SIZEOF_HSWORD)
deriveElt(Word8, W8#, readWord8Array#, writeWord8Array#, indexWord8Array#, SIZEOF_WORD8)
deriveElt(Word16, W16#, readWord16Array#, writeWord16Array#, indexWord16Array#, SIZEOF_WORD16)
deriveElt(Word32, W32#, readWord32Array#, writeWord32Array#, indexWord32Array#, SIZEOF_WORD32)
deriveElt(Word64, W64#, readWord64Array#, writeWord64Array#, indexWord64Array#, SIZEOF_WORD64)
deriveElt(Int, I#, readIntArray#, writeIntArray#, indexIntArray#, SIZEOF_HSINT)
deriveElt(Int8, I8#, readInt8Array#, writeInt8Array#, indexInt8Array#, SIZEOF_INT8)
deriveElt(Int16, I16#, readInt16Array#, writeInt16Array#, indexInt16Array#, SIZEOF_INT16)
deriveElt(Int32, I32#, readInt32Array#, writeInt32Array#, indexInt32Array#, SIZEOF_INT32)
deriveElt(Int64, I64#, readInt64Array#, writeInt64Array#, indexInt64Array#, SIZEOF_INT64)
deriveElt(Float, F#, readFloatArray#, writeFloatArray#, indexFloatArray#, SIZEOF_HSFLOAT)
deriveElt(Double, D#, readDoubleArray#, writeDoubleArray#, indexDoubleArray#, SIZEOF_HSDOUBLE)
deriveElt(Char, C#, readWideCharArray#, writeWideCharArray#, indexWideCharArray#, SIZEOF_HSCHAR)

#else

withMArrayPtr :: MutableByteArray s -> (Ptr a -> IO b) -> IO b
withArrayPtr  :: ByteArray -> (Ptr a -> IO b) -> IO b

withMArrayPtr (MutableByteArray fptr _) k = withForeignPtr (castForeignPtr fptr) k
withArrayPtr (ByteArray fptr _) k = withForeignPtr (castForeignPtr fptr) k

new n = unsafeIOToST $ flip MutableByteArray n `fmap` mallocForeignPtrBytes n
newPinned = new -- FFI arrays already pinned

unsafeFreeze (MutableByteArray fptr n)
    = return . flip ByteArray n $ fptr

asPtr = withArrayPtr

length (ByteArray _ n) = n
lengthM (MutableByteArray _ n) = n

hashByteArray arr =
    unsafePerformIO $
    withArrayPtr arr $ \ptr ->
    H.hashPtr ptr 0 (length arr)

#define deriveElt(Typ) \
instance Elt Typ where { \
    read ary ndx \
        = unsafeIOToST $ withMArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   write ary ndx word \
        = unsafeIOToST $ withMArrayPtr ary $ \ptr -> pokeElemOff ptr ndx word \
;   index ary ndx = unsafePerformIO $ withArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   elemSize = sizeOf \
}

deriveElt(Word)
deriveElt(Word8)
deriveElt(Word16)
deriveElt(Word32)
deriveElt(Word64)
deriveElt(Int)
deriveElt(Int8)
deriveElt(Int16)
deriveElt(Int32)
deriveElt(Int64)
deriveElt(Float)
deriveElt(Double)
deriveElt(Char)

#endif
