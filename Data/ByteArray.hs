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
    , newByteArray
    , newPinnedByteArray
    , Elem(..)
    , unsafeFreezeByteArray
    , byteArrayContents
    ) where


#if defined(USING_GHC)
-- for GHC we re-use the work done in the primitve
-- package, specialised to Word8/IO
import qualified Data.Primitive as P
import qualified Control.Monad.Primitive as P

import GHC.Exts
import GHC.IOBase (IO(..))

#else
import Foreign
#endif

import Data.Word
import Data.Int

import Control.Monad.ST

#if defined(USING_GHC)
newtype ByteArray = ByteArray {unArray :: P.ByteArray }
newtype MutableByteArray s = MutableByteArray {unMArray :: P.MutableByteArray s }
#else
-- fallback to FFI foreign pointers
newtype ByteArray = ByteArray (ForeignPtr Word8)
newtype MutableByteArray s = MutableByteArray (ForeignPtr Word8)
#endif

newByteArray :: Int -> ST s (MutableByteArray s)
newPinnedByteArray :: Int -> ST s (MutableByteArray s)
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray

class Elem a where
    readByteArray  :: MutableByteArray s -> Int -> ST s a
    writeByteArray :: MutableByteArray s -> Int -> a -> ST s ()
    indexByteArray :: ByteArray -> Int -> a
    elemSize       :: a -> Int

-- | Only for use with pinned arrays! Otherwise very unsafe.
byteArrayContents :: ByteArray -> (Ptr a -> IO b) -> IO b

#if defined(USING_GHC)

newByteArray n = MutableByteArray `fmap` P.newByteArray n
newPinnedByteArray n = MutableByteArray `fmap` P.newPinnedByteArray n
unsafeFreezeByteArray (MutableByteArray ary) = ByteArray `fmap` P.unsafeFreezeByteArray ary

byteArrayContents (ByteArray ary) k
    = case P.byteArrayContents ary of
        P.Addr addr# -> do
          x <- k $ Ptr addr#
          touch ary
          return x

touch :: a -> IO ()
touch x = IO $ \s-> case touch# x s of s' -> (# s', () #)

#define deriveElem(Typ) \
instance Elem Typ where { \
    readByteArray ary n = P.readByteArray (unMArray ary) n \
;   writeByteArray ary n b = P.writeByteArray (unMArray ary) n b \
;   indexByteArray ary n = P.indexByteArray (unArray ary) n \
;   elemSize x = I# (P.sizeOf# x) \
}


#else

withMArrayPtr :: MutableByteArray s -> (Ptr a -> IO b) -> IO b
withArrayPtr  :: ByteArray -> (Ptr a -> IO b) -> IO b

withMArrayPtr (MutableByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k
withArrayPtr (ByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k

newByteArray n = unsafeIOToST $ MutableByteArray `fmap` mallocForeignPtrBytes n
newPinnedByteArray = newByteArray -- FFI arrays already pinned

unsafeFreezeByteArray (MutableByteArray fptr)
    = return . ByteArray $ fptr

byteArrayContents = withArrayPtr

#define deriveElem(Typ) \
instance Elem Typ where { \
    readByteArray ary ndx \
        = unsafeIOToST $ withMArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   writeByteArray ary ndx word \
        = unsafeIOToST $ withMArrayPtr ary $ \ptr -> pokeElemOff ptr ndx word \
;   indexByteArray ary ndx = unsafePerformIO $ withArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   elemSize = sizeOf \
}


#endif

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
