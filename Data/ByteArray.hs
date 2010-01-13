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

You may be thinking to yourself: \"Self, this looks a lot like a foreign pointer from
the FFI. Why should I use this?\"
Haskell implementations may implement raw memory buffers that are faster than FFI
foreign pointer access.
This library takes care of figuring that out for you, and falling back to FFI foreign
pointers when we can't - so your library can be portable.

-}

module Data.ByteArray
    ( ByteArray
    , MutableByteArray
    , new
    , newPinned
    , Elem(..)
    , unsafeFreeze
    , asPtr
    ) where


#if defined(USING_GHC)
-- for GHC we use GHC.Prim byte arrays
import GHC.Exts
import GHC.IO (IO(..))
import GHC.ST (ST(..))

import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))

#include "MachDeps.h"

#else
import Foreign hiding (new)
#endif

import Control.Monad.ST

#if defined(USING_GHC)
data ByteArray = ByteArray {unArray :: ByteArray# }
data MutableByteArray s = MutableByteArray {unMArray :: MutableByteArray# s}
#else
-- fallback to FFI foreign pointers
newtype ByteArray = ByteArray (ForeignPtr Word8)
newtype MutableByteArray s = MutableByteArray (ForeignPtr Word8)
#endif

-- | Allocate a new array. The size is specified in bytes.
new :: Int -> ST s (MutableByteArray s)
-- | Allocate a new array in a memory region which will not
-- be moved. The size is specified in bytes.
newPinned :: Int -> ST s (MutableByteArray s)

-- | Convert a MutableByteArray to a ByteArray. You should
-- not modify the source array after calling this.
unsafeFreeze :: MutableByteArray s -> ST s ByteArray

class Elem a where
    read     :: MutableByteArray s -> Int -> ST s a
    write    :: MutableByteArray s -> Int -> a -> ST s ()
    index    :: ByteArray -> Int -> a

    -- | The size of an element in bytes
    elemSize :: a -> Int

-- | Only for use with pinned arrays.
asPtr :: ByteArray -> (Ptr a -> IO b) -> IO b

#if defined(USING_GHC)

new (I# n#)
    = ST $ \s -> case newByteArray# n# s of
                   (# s', ary #) -> (# s', MutableByteArray ary #)
newPinned (I# n#)
    = ST $ \s -> case newPinnedByteArray# n# s of
                   (# s', ary #) -> (# s', MutableByteArray ary #)

unsafeFreeze (MutableByteArray mary)
    = ST $ \s -> case unsafeFreezeByteArray# mary s of
                   (# s', ary #) -> (# s', ByteArray ary #)

asPtr a@(ByteArray ary) k
    = case byteArrayContents# ary of
        addr# -> do
          x <- k $ Ptr addr#
          touch a
          return x

touch :: a -> IO ()
touch x = IO $ \s-> case touch# x s of s' -> (# s', () #)

#define deriveElem(Typ, Ct, rd, wrt, ix, sz) \
instance Elem Typ where { \
    read ary (I# n) = ST (\s -> case rd (unMArray ary) n s of \
                                   {(# s', b #) -> (# s', Ct b #)}) \
;   write ary (I# n) (Ct b) = ST (\s -> (# wrt (unMArray ary) n b s, () #)) \
;   index ary (I# n) = Ct (ix (unArray ary) n) \
;   elemSize _ = sz \
}


deriveElem(Word, W#, readWordArray#, writeWordArray#, indexWordArray#, SIZEOF_HSWORD)
deriveElem(Word8, W8#, readWord8Array#, writeWord8Array#, indexWord8Array#, SIZEOF_WORD8)
deriveElem(Word16, W16#, readWord16Array#, writeWord16Array#, indexWord16Array#, SIZEOF_WORD16)
deriveElem(Word32, W32#, readWord32Array#, writeWord32Array#, indexWord32Array#, SIZEOF_WORD32)
deriveElem(Word64, W64#, readWord64Array#, writeWord64Array#, indexWord64Array#, SIZEOF_WORD64)
deriveElem(Int, I#, readIntArray#, writeIntArray#, indexIntArray#, SIZEOF_HSINT)
deriveElem(Int8, I8#, readInt8Array#, writeInt8Array#, indexInt8Array#, SIZEOF_INT8)
deriveElem(Int16, I16#, readInt16Array#, writeInt16Array#, indexInt16Array#, SIZEOF_INT16)
deriveElem(Int32, I32#, readInt32Array#, writeInt32Array#, indexInt32Array#, SIZEOF_INT32)
deriveElem(Int64, I64#, readInt64Array#, writeInt64Array#, indexInt64Array#, SIZEOF_INT64)
deriveElem(Float, F#, readFloatArray#, writeFloatArray#, indexFloatArray#, SIZEOF_HSFLOAT)
deriveElem(Double, D#, readDoubleArray#, writeDoubleArray#, indexDoubleArray#, SIZEOF_HSDOUBLE)
deriveElem(Char, C#, readWideCharArray#, writeWideCharArray#, indexWideCharArray#, SIZEOF_HSCHAR)

#else

withMArrayPtr :: MutableByteArray s -> (Ptr a -> IO b) -> IO b
withArrayPtr  :: ByteArray -> (Ptr a -> IO b) -> IO b

withMArrayPtr (MutableByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k
withArrayPtr (ByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k

new n = unsafeIOToST $ MutableByteArray `fmap` mallocForeignPtrBytes n
newPinned = new -- FFI arrays already pinned

unsafeFreeze (MutableByteArray fptr)
    = return . ByteArray $ fptr

asPtr = withArrayPtr

#define deriveElem(Typ) \
instance Elem Typ where { \
    read ary ndx \
        = unsafeIOToST $ withMArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   write ary ndx word \
        = unsafeIOToST $ withMArrayPtr ary $ \ptr -> pokeElemOff ptr ndx word \
;   index ary ndx = unsafePerformIO $ withArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   elemSize = sizeOf \
}

deriveElem(Word)
deriveElem(Word8)
deriveElem(Word16)
deriveElem(Word32)
deriveElem(Word64)
deriveElem(Int)
deriveElem(Int8)
deriveElem(Int16)
deriveElem(Int32)
deriveElem(Int64)
deriveElem(Float)
deriveElem(Double)
deriveElem(Char)

#endif
