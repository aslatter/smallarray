{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

#ifndef TESTING_FFI
#if defined(__GLASGOW_HASKELL__)
#define USING_GHC
#endif
#endif


{-# LANGUAGE MagicHash #-}

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
    , STByteArray
    , IOByteArray
    , Elem(..)
    , MByteArray(..)
    , MElem(..)
    , byteArrayContents
    ) where

#ifndef TESTING_FFI

#if defined(__GLASGOW_HASKELL__)
#define USING_GHC
#endif

#endif

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

#if defined(USING_GHC)
newtype ByteArray = ByteArray {unArray :: P.ByteArray }
newtype MutableByteArray = MutableByteArray {unMArray :: P.MutableByteArray P.RealWorld }
#else
-- fallback to FFI foreign pointers
newtype ByteArray = ByteArray (ForeignPtr Word8)
newtype MutableByteArray = MutableByteArray (ForeignPtr Word8)
#endif

newByteArray :: Int -> IO MutableByteArray
newPinnedByteArray :: Int -> IO MutableByteArray
unsafeFreezeByteArray :: MutableByteArray -> IO ByteArray

class Elem a where
    readByteArray  :: MutableByteArray -> Int -> IO a
    writeByteArray :: MutableByteArray -> Int -> a -> IO ()
    indexByteArray :: ByteArray -> Int -> a
    elemSize       :: a -> Int

-- | Only for use with pinned arrays! Otherwise very unsafe.
byteArrayContents :: ByteArray -> (Ptr a -> IO b) -> IO b

#if defined(USING_GHC)

instance MByteArray (STByteArray s) (ST s) where
    newByteArray n = STByteArray `fmap` P.newByteArray n
    newPinnedByteArray n = STByteArray `fmap` P.newPinnedByteArray n
    unsafeFreezeByteArray (STByteArray a) = ByteArray `fmap` P.unsafeFreezeByteArray a

instance MByteArray IOByteArray IO where
    newByteArray n = IOByteArray `fmap` P.newByteArray n
    newPinnedByteArray n = IOByteArray `fmap` P.newPinnedByteArray n
    unsafeFreezeByteArray (IOByteArray a) = ByteArray `fmap` P.unsafeFreezeByteArray a

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
    indexByteArray ary n = P.indexByteArray (unArray ary) n \
;   elemSize x = I# (P.sizeOf# x) \
}

#define deriveElemST(Type) \
instance MElem (STByteArray s) (ST s) Type where { \
    readByteArray ary n = P.readByteArray (unSTArray ary) n \
;   writeByteArray ary n b = P.writeByteArray (unSTArray ary) n b \
}

#define deriveElemIO(Type) \
instance MElem IOByteArray IO Type where { \
    readByteArray ary n = P.readByteArray (unIOArray ary) n \
;   writeByteArray ary n b = P.writeByteArray (unIOArray ary) n b \
}

#else

withMArrayPtr :: MutableByteArray -> (Ptr a -> IO b) -> IO b
withArrayPtr  :: ByteArray -> (Ptr a -> IO b) -> IO b

withSTArrayPtr (STByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k
withIOArrayPtr (IOByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k
withArrayPtr (ByteArray fptr) k = withForeignPtr (castForeignPtr fptr) k

newByteArray n = MutableByteArray `fmap` mallocForeignPtrBytes n
newPinnedByteArray = newByteArray -- FFI arrays already pinned


byteArrayContents = withArrayPtr

#define deriveElem(Typ) \
instance Elem Typ where { \
    readByteArray ary ndx = withMArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   writeByteArray ary ndx word = withMArrayPtr ary $ \ptr -> pokeElemOff ptr ndx word \
;   indexByteArray ary ndx = unsafePerformIO $ withArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   elemSize = sizeOf \
}

#define deriveElemST(Typ) \
instance MElem (STByteArray s) (ST s) Typ where { \
    readByteArray ary ndx = unsafeIOToST $ withSTArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   writeByteArray ary ndx word \
     = unsafeIOToST $ withSTArrayPtr ary $ \ptr -> pokeElemOff ptr ndx word \
}

#define deriveElemIO(Typ) \
instance MElem IOByteArray IO Typ where { \
    readByteArray ary ndx = withIOArrayPtr ary $ \ptr -> peekElemOff ptr ndx \
;   writeByteArray ary ndx word = withIOArrayPtr ary $ \ptr -> pokeElemOff ptr ndx word \
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

deriveElemST(Word)
deriveElemST(Word8)
deriveElemST(Word16)
deriveElemST(Word32)
deriveElemST(Word64)
deriveElemST(Int)
deriveElemST(Int8)
deriveElemST(Int16)
deriveElemST(Int32)
deriveElemST(Int64)
deriveElemST(Float)
deriveElemST(Double)
deriveElemST(Char)

deriveElemIO(Word)
deriveElemIO(Word8)
deriveElemIO(Word16)
deriveElemIO(Word32)
deriveElemIO(Word64)
deriveElemIO(Int)
deriveElemIO(Int8)
deriveElemIO(Int16)
deriveElemIO(Int32)
deriveElemIO(Int64)
deriveElemIO(Float)
deriveElemIO(Double)
deriveElemIO(Char)
